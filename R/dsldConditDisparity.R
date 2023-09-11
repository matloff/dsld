
dsldConditDisparity <- function(data, yName, sName, xName, condits,
                                qeFtn = qeKNN, minS = 50, yLim = NULL,
                                useLoess = TRUE) {
    # args type checking#
    if (!is.data.frame(data)) {
        stop("data must be a dataframe.")
    }
    if (!is.numeric(data[[yName]])) {
        stop("yName must refer to a numeric column in data.")
    }
    if (!is.factor(data[[sName]])) {
        stop("sName must refer to a factor column in data.")
    }
    if (!is.numeric(data[[xName]])) {
        stop("xName must refer to a numeric column in data.")
    }

    # function setup #
    # library requirements
    library(qeML)

    # fill plotting limits
    if (is.null(yLim)) {
        yLim <- c(0, max(data[[yName]]))                    # [0, max(y)]
    }

    
    # data engineering #
    # restrict data to fit conditions
    if (length(condits) > 1) {
        # combine conditions
        condits <- paste(condits, collapse = ' & ')
    }
    restrictions <- sprintf('focusedData <- subset(data, %s)', condits)
    eval(parse(text = restrictions))

    # won't use the restricting variables anymore
    focusedData <- focusedData[c(yName, xName, sName)]
    sCol <- which(names(focusedData) == sName)

    # group the data by S level & execute min size condition
    s <- focusedData[[sName]]
    groupByS <- split(focusedData, s)
    sizes <- sapply(groupByS, nrow)
    tiny <- which(sizes < minS)

    # remove too small groups
    if (length(tiny) > 0)
    {
        groupByS <- groupByS[-tiny]
    }


    # plotting #
    # consider only the remaining S-levels
    sLevels <- names(groupByS)
    remainingS <- length(sLevels)
    colors <- colorRampPalette(c("blue", "red"))(remainingS)

    # plot each sensitive var wrt x
    for (i in 1:remainingS) {
        # setup data for training
        curData <- groupByS[[i]][,-sCol]                    # data for current s-level w/o sensitive column
        curXData <- unique(curData[[xName]])                # data for only the numeric x column
        curXDF <- as.data.frame(curXData)                   # x-data as a dataframe
        names(curXDF) <- xName                              # adjust column name

        # fit ML model
        model <- qeFtn(curData, yName, holdout=NULL)        # `holdout=NULL` to best predict [overfit] dataset
        preds <- predict(model, curXDF)

        # sort data for time series plotting
        curXData <- as.vector(curXData)
        preds <- as.vector(preds)
        orderedXData <- order(curXData)

        curXData <- curXData[orderedXData]
        preds <- preds[orderedXData]
        plotdf <- data.frame(curXData, preds)               # store dataframe w/ sorted data for plotting

        # check Loess
        if (useLoess) {
            preds <- loess(preds ~ curXData, plotdf)$fitted # loess smoothing
        }

        # plotting method
        if (i == 1) {
            # create plot
            plot(
                curXData,
                preds,
                type = "l",
                lty = "solid",
                ylim = yLim,
                col = colors[i],
                xlab = xName,
                ylab = yName,
                main = paste("Underlying Effects of ", sName, " on ", yName, " wrt ", xName)
            )

            # create legend
            legend(
                x = "bottomright",
                lty = c(4,6),
                text.font = 4,
                col = colors,
                text.col = "black",
                legend = sLevels
            )
        } else {
            # plot points
            points(curXData, preds, type = "l", lty = "solid", col = colors[i])
        }
    }
}

