
# many functions in dsld are wrappers for functions in other packages;
# in order to avoid "package bloat," we instead check for them as needed

# e.g. say a dsld function f() wraps some function in package p; then
# instead of listing p as imported etc. in the dsld DESCRIPTION file, 
# we write the top of f(), getSuggestedLib('p'); this loads p if it is
# installed on the user's machine, otherwise so informs the user

getSuggestedLib <- function(pkgName) {
   if (!requireNamespace(pkgName,quietly=TRUE))
      stop(paste0(pkgName, ' not loaded'))
}

pr2file <- function(filename)
{
   origdev <- dev.cur()
   parts <- strsplit(filename,".",fixed=TRUE)
   nparts <- length(parts[[1]])
   suff <- parts[[1]][nparts]
   if (suff == "pdf") {
       pdf(filename)
   }
   else if (suff == "png") {
       png(filename,bg='white')
   }
   else jpeg(filename)
   devnum <- dev.cur()
   dev.set(origdev)
   dev.copy(which = devnum)
   dev.set(devnum)
   dev.off()
   dev.set(origdev)
}

# generates a "cartesian product" of factor levels from input factors
cartFactorLvls <- function(factorNames) 
{
   theLevels <- lapply(factorNames,function(fName) levels(get(fName)))
   expand.grid(theLevels)
}

## needed for dsldLinear, dsldLogit -------------------------------------------
### selects 5 rows for comparison across each level of the sensitive variable
### randomly if the user doesn't supply data in the interactions case

dsldGetRow5 <- function(data, yName, sName) {
  rows <- sample(nrow(data), 5)
  reducedData <- data[rows, ]
  columns <- c(yName, sName)
  newDat <- reducedData[, !(names(reducedData) %in% columns)]
  result <- sprintf("No user sComparisonPts supplied. The following rows 
                    are selected: %s,%s,%s,%s,%s", rows[1],rows[2],rows[3],rows[4],
                    rows[5]); print(result)
  return(newDat)
}

## needed for: python interfaces ----------------------------------------------
### convert data to factors and numeric as per user input
convert_cols <- function(data, cat_features = character(), num_features = character()) {
  # If both vectors are missing or empty, return original data unchanged
  if ((missing(cat_features) || length(cat_features) == 0) &&
      (missing(num_features) || length(num_features) == 0)) {
    return(data)
  }
  
  data[] <- lapply(names(data), function(col) {
    if (col %in% cat_features) {
      factor(data[[col]])
    } else if (col %in% num_features) {
      as.numeric(data[[col]])
    } else {
      data[[col]]
    }
  })
  
  names(data) <- names(data)  # preserve original column names
  data
}

### stores factors levels for each factor in dataset
factor_levels <- function(data) {
  stopifnot(is.data.frame(data))
  facs <- names(Filter(is.factor, data))
  setNames(lapply(facs, function(nm) levels(data[[nm]])), facs)
}

### applies factor levels from each factor in dataset
apply_factor_levels <- function(test_data, train_levels, quiet = TRUE) {
  stopifnot(is.data.frame(test_data), is.list(train_levels))
  cols <- intersect(names(train_levels), names(test_data))
  if (!quiet) {
    skipped <- setdiff(names(train_levels), names(test_data))
    if (length(skipped)) message("Skipping missing columns: ", paste(skipped, collapse = ", "))
  }
  
  out <- test_data
  for (nm in cols) {
    levs <- train_levels[[nm]]
    v <- out[[nm]]
    v_chr <- as.character(v)                   # works for factor/char/anything coercible
    fac <- factor(v_chr, levels = levs)        # unseen -> NA
    if (!quiet && all(is.na(fac))) {
      warning("Column '", nm, "' became all NA after applying training levels.")
    }
    out[[nm]] <- fac
  }
  out
}

## needed for fairML and EDF-Fair functions -----------------------------------
### converts integer cols to numeric and character cols to factors
toNumericFactor <- function(data) {
  data[,unlist(lapply(data, is.integer))] <- 
    lapply(data[,unlist(lapply(data, is.integer))], as.numeric)
  data[,unlist(lapply(data, is.character))] <- 
    lapply(data[,unlist(lapply(data, is.character))], as.factor)
  data
}

### computes correlation between predictions and one or more sensitive attributes 
s_correlations <- function(data, sNames, predictions,
                           method = "pearson",
                           sort_by_abs = TRUE) {
  stopifnot(is.data.frame(data))
  
  # normalize sNames
  if (length(sNames) == 1L && is.character(sNames) && grepl(",", sNames, fixed = TRUE)) {
    sNames <- trimws(strsplit(sNames, ",", fixed = TRUE)[[1]])
  }
  sNames <- unique(as.character(sNames[nzchar(sNames)]))
  
  if (length(predictions) != nrow(data)) {
    stop("`predictions` length (", length(predictions),
         ") must equal nrow(data) (", nrow(data), ").")
  }
  y <- as.numeric(predictions)
  
  blocks <- list()
  
  for (s in sNames) {
    if (!s %in% names(data)) {
      warning("Skipping missing column: ", s)
      next
    }
    v <- data[[s]]
    
    # coerce characters to factors
    if (is.character(v)) v <- factor(v)
    
    if (is.factor(v)) {
      v <- droplevels(v)
      # If all NA or fewer than 2 levels, skip to avoid contrasts error
      if (all(is.na(v))) {
        warning("Skipping '", s, "': all values are NA after level alignment.")
        next
      }
      if (nlevels(v) < 2L) {
        warning("Skipping '", s, "': factor has fewer than 2 observed levels.")
        next
      }
      mm <- model.matrix(~ v - 1)              # safe now
      colnames(mm) <- paste0(s, "==", levels(v))
      blocks[[s]] <- mm
      
    } else if (is.logical(v)) {
      blocks[[s]] <- matrix(as.numeric(v), ncol = 1, dimnames = list(NULL, s))
      
    } else if (is.numeric(v) || is.integer(v)) {
      blocks[[s]] <- matrix(as.numeric(v), ncol = 1, dimnames = list(NULL, s))
      
    } else {
      warning("Skipping unsupported type for ", s,
              " (class: ", paste(class(v), collapse = "/"), ")")
    }
  }
  
  if (!length(blocks)) {
    return(data.frame(feature = character(0), correlation = numeric(0)))
  }
  
  X <- do.call(cbind, blocks)
  
  cors <- vapply(seq_len(ncol(X)),
                 function(j) cor(y, X[, j], use = "pairwise.complete.obs", method = method),
                 numeric(1))
  names(cors) <- colnames(X)
  
  if (isTRUE(sort_by_abs)) cors <- cors[order(abs(cors), decreasing = TRUE)]
  
  data.frame(feature = names(cors), correlation = as.numeric(cors), row.names = NULL)
}
