
# useful helpers
### create k-fold split
make_folds <- function(n, k = 5) {
  stopifnot(n >= k, k >= 2)
  idx <- sample.int(n)                                 # shuffle rows
  split(idx, cut(seq_along(idx), breaks = k, labels = FALSE))
}

get_fold_split <- function(data, folds, i) {
  stopifnot(i >= 1, i <= length(folds))
  test_idx  <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(data)), test_idx)
  list(
    train = data[train_idx, , drop = FALSE],
    test  = data[test_idx,  , drop = FALSE]
  )
}

# Helper: keep only arguments that the target function supports (by name)
.filter_model_args <- function(ftn_name, user_args) {
  if (is.null(user_args) || !length(user_args)) return(list())
  ftn <- match.fun(ftn_name)
  allowed <- names(formals(ftn))
  keep <- names(user_args) %in% allowed
  if (any(!keep)) {
    dropped <- names(user_args)[!keep]
    warning(sprintf("Ignoring unsupported arg(s) for %s: %s",
                    ftn_name, paste(dropped, collapse = ", ")),
            call. = FALSE)
  }
  user_args[keep]
}

dsldFairUtils <- function(data, yName, sName, dsldFTNName, unfairness = NULL, 
                          deweightPars = NULL, yesYVal = NULL, k_folds = 5, 
                          model_args = NULL) {
  
  valid_models <- c("dsldQeFairKNN", "dsldQeFairRF", "dsldQeFairRidgeLin", "dsldQeFairRidgeLog", 
                    "dsldFrrm", "dsldFgrrm", "dsldNclm", "dsldZlm", "dsldZlrm")
  if (!(dsldFTNName %in% valid_models)) stop("Invalid dsldFTNName specified")
  
  # classification gating
  if (is.factor(data[[yName]])) {
    if (is.null(yesYVal)) stop("missing yesYVal")
    data[[yName]] <- as.factor(as.numeric(data[[yName]] == yesYVal))
    yesYVal <- "1"
  }
  
  if (dsldFTNName %in% c("dsldQeFairKNN","dsldQeFairRF","dsldQeFairRidgeLin","dsldQeFairRidgeLog")) {
   
    # --- build grid of deweightPars combos ---
    if (is.null(deweightPars) || !length(deweightPars))
      stop("Provide deweightPars as a named list. For a grid, use vectors (e.g. list(occ=c(0.2,0.4), educ=c(0.4))).")
    
    # If any element has length > 1, treat as grid; else one single combo
    is_grid <- any(vapply(deweightPars, length, integer(1)) > 1)
    grid_df <- if (is_grid) {
      expand.grid(deweightPars, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    } else {
      # single row data.frame so we can reuse the same loop
      as.data.frame(as.list(deweightPars), stringsAsFactors = FALSE)
    }
    
    # Pre-filter user-supplied model args to only those supported by the target function
    extra <- .filter_model_args(dsldFTNName, model_args)
    
    rows <- vector("list", nrow(grid_df))
    
    # Cache folds once (same across combos)
    folds <- make_folds(nrow(data), k = k_folds)
    
    # Inner CV runner for one deweight combination (named numeric list)
    run_one_combo_1 <- function(dw_list_named) {
      accs <- numeric(length(folds))
      corr_sums <- NULL
      feat_names <- NULL
      
      for (i in seq_along(folds)) {
        split <- get_fold_split(data, folds, i)
        trn <- split$train
        tst <- split$test
        y_test <- tst[[yName]]
        tst_x <- tst[, setdiff(names(tst), yName), drop = FALSE]
        
        base_args <- list(
          data         = trn,
          yName        = yName,
          sNames       = sName,
          deweightPars = dw_list_named
        )
        
        if (!is.null(yesYVal)) base_args$yesYVal <- yesYVal
        
        call_args <- utils::modifyList(base_args, extra, keep.null = TRUE)
        fitted <- do.call(dsldFTNName, call_args)
        
        res   <- predict(fitted, tst_x)
        corrs <- res$correlations   # data.frame: feature, correlation
        
        if (is.null(feat_names)) {
          feat_names <- as.character(corrs$feature)
          corr_sums  <- setNames(numeric(length(feat_names)), feat_names)
        }
        corr_sums[as.character(corrs$feature)] <-
          corr_sums[as.character(corrs$feature)] + corrs$correlation
        
        if (!is.null(yesYVal)) {
          accs[i] <- mean(y_test != as.integer(res$preds$probs > 0.5))
        } else {
          accs[i] <- mean(abs(res$preds - y_test))
        }
      }
      
      # averages for this combo
      mean_acc   <- mean(accs)
      mean_corrs <- corr_sums / length(folds)
      
      # return as named list: testAcc + correlation columns
      c(list(testAcc = mean_acc), as.list(mean_corrs))
    }
    
    # Loop over each row of the grid
    for (r in seq_len(nrow(grid_df))) {
      
      dw_row <- lapply(grid_df[r, , drop = FALSE], function(x) as.numeric(x)[1])
      names(dw_row) <- names(grid_df)
      
      metrics <- run_one_combo_1(dw_row)
      
      # Build a single row: params first, then metrics; keep raw names for correlation columns
      rows[[r]] <- as.data.frame(c(as.list(dw_row), metrics), check.names = FALSE)
    }
    
    # Bind all rows; ensure parameter columns come first
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
   
  
    # fairML wrappers 
  } else {
    if (is.null(unfairness) || !length(unfairness))
      stop("Provide unfairness as a vector of numbers between (0,1]. For example: unfairness = c(0.2, 0.9).")
    
    # Pre-filter user-supplied model args to only those supported by the target function
    extra <- .filter_model_args(dsldFTNName, model_args)
    
    # Cache folds once (same across combos)
    folds <- make_folds(nrow(data), k = k_folds)
    
    # Inner CV runner for one unfairness value
    run_one_combo_2 <- function(u_val) {
      accs <- numeric(length(folds))
      corr_sums <- NULL
      feat_names <- NULL
      
      for (i in seq_along(folds)) {
        split <- get_fold_split(data, folds, i)
        trn <- split$train
        tst <- split$test
        y_test <- tst[[yName]]
        tst_x <- tst[, setdiff(names(tst), yName), drop = FALSE]
        
        base_args <- list(
          data       = trn,
          yName      = yName,
          sName      = sName,
          unfairness = u_val
        )
        if (!is.null(yesYVal)) base_args$yesYVal <- yesYVal
        
        call_args <- utils::modifyList(base_args, extra, keep.null = TRUE)
        fitted <- do.call(dsldFTNName, call_args)
     
        res   <- predict(fitted, tst_x)
        corrs <- res$correlations   # data.frame: feature, correlation
        
        if (is.null(feat_names)) {
          feat_names <- as.character(corrs$feature)
          corr_sums  <- setNames(numeric(length(feat_names)), feat_names)
        }
        corr_sums[as.character(corrs$feature)] <-
          corr_sums[as.character(corrs$feature)] + corrs$correlation
        
        if (!is.null(yesYVal)) {
          accs[i] <- mean(y_test != as.integer(res$preds > 0.5))
        } else {
          accs[i] <- mean(abs(res$preds - y_test))
        }
      }
      
      # averages for this unfairness value
      mean_acc   <- mean(accs)
      mean_corrs <- corr_sums / length(folds)
      
      # return as named list: testAcc + correlation columns
      c(list(testAcc = mean_acc), as.list(mean_corrs))
    }
    
    # Loop over the unfairness vector (no grid_df here)
    rows <- vector("list", length(unfairness))
    for (u_idx in seq_along(unfairness)) {
      unfairVal <- as.numeric(unfairness[u_idx])
      m <- as.list(run_one_combo_2(unfairVal))  # named list: testAcc + corr cols
      
      rows[[u_idx]] <- data.frame(
        c(
          list(unfairness = unfairVal, testAcc = m$testAcc),
          m[setdiff(names(m), "testAcc")]
        ),
        check.names = FALSE
      )
    }
    
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
}



  
