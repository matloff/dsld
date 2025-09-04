dsldConfounders <- function(data, sName, graphType = "plotly", fill = FALSE) {
    # Error checking
    if (is.null(sName)) {
        stop(paste("sName must be provided as a quoted column name"))
    }

    # dispatch to appropriate auxiliary method
    numCols <- ncol(data)
    for (i in 1:numCols) {
        # skip sName
        if (colnames(data)[i] == sName) {
             next
        }

        # if categorical
        if (is.factor(data[, i])) {
            print(dsldFrequencyByS(data, colnames(data)[i], sName))

            # require input if there's a next
            if (i != numCols) {
                cat("Press <ENTER> to view next density graph / frequency dataframe...\n")
                tempInput <- readline()
            }
        # if numeric
        } else if (is.numeric(data[, i])) {
            print(dsldDensityByS(data, colnames(data)[i], sName, graphType, fill))

            # require input if there's a next
            if (i != numCols) {
                cat("Press <ENTER> to view next density graph / frequency dataframe...\n")
                tempInput <- readline()
            }
        # throw error
        } else {
            stop(paste("Neither categorical or numeric column, check dataframe"))
        }
    }
}

