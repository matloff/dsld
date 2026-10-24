
dsldFreqPCoord <- function(data, m, sName = NULL,
                           method = "maxdens", faceting = "vert", k = 50,
                           klm = 5 * k, keepidxs = NULL, plotidxs = FALSE,
                           cls = NULL, plot_filename = NULL) {

    getSuggestedLib("freqparcoord")
    getSuggestedLib("ggplot2")
    if (is.matrix(data)) data <- as.data.frame(data)
    .dsld_check_data(data, if (is.null(sName)) names(data) else sName,
                     complete = TRUE)
    data <- droplevels(toNumericFactor(data))

    if (!is.null(sName)) {
       group_values <- data[[sName]]
       predictor_data <- data[, setdiff(names(data), sName), drop = FALSE]
    } else {
       predictor_data <- data
    }
    # Constant axes carry no information and cause the backend's scaling to fail.
    varying <- vapply(predictor_data, function(column) length(unique(column)) > 1L,
                       logical(1))
    if (any(!varying)) warning("Omitting constant parallel-coordinate columns: ",
      paste(names(predictor_data)[!varying], collapse = ", "), call. = FALSE)
    predictor_data <- predictor_data[, varying, drop = FALSE]
    if (!ncol(predictor_data)) stop("Parallel coordinates need at least one varying column.")
    data <- as.data.frame(factorsToDummies(predictor_data))
    # The backend standardizes again inside each group for density estimation.
    # Even a globally varying indicator can be constant within a group. Detect
    # that before floating-point scaling, whose zero-variance behavior differs
    # across BLAS/R builds, and explain which axes cannot be used.
    groups <- if (!is.null(sName) && !identical(method, "locmax")) {
        split(seq_len(nrow(data)), group_values)
    } else list(seq_len(nrow(data)))
    varying_within_groups <- vapply(data, function(column) {
        all(vapply(groups, function(rows) length(unique(column[rows])) > 1L,
                   logical(1)))
    }, logical(1))
    if (any(!varying_within_groups)) {
        warning("Omitting parallel-coordinate axes constant within a plotted group: ",
          paste(names(data)[!varying_within_groups], collapse = ", "), call. = FALSE)
    }
    columns <- unname(which(varying_within_groups))
    if (!length(columns)) {
        stop("Parallel coordinates need a predictor that varies within every plotted group.")
    }
    if (!is.null(sName)) data[[sName]] <- group_values

    fpcOut <- freqparcoord::freqparcoord(
        data,
        m,
        dispcols = columns,
        grpvar = sName,
        method = method,
        faceting = faceting,
        k = k,
        klm = klm,
        keepidxs = keepidxs,
        plotidxs = plotidxs,
        cls = cls
    )

    if (!is.null(plot_filename)) {
        ggplot2::ggsave(plot_filename, fpcOut) # Save as img
    }

    return(fpcOut)
}
