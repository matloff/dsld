
# arguments

#   data: input data frame or equivalewnt
#   yName: response variable
#   sName: sensitive variable (R factor)
#   xName: horizontal axis variables
#   condits: conditions, a vector of conditions, expressed in
#      names(data); must have at least 1, even if trivial
#   qeFtn: qeML predictive function
#   minS: if 'data' has fewer than this many rows for a give S level,
#      don't use that level
#   useLoess: if TRUE, use loess smoothing

dsldConditDisparity <- function(data, yName, sName, xName, condits = NULL,
                                qeFtn = qeKNN, minS = 50, useLoess = TRUE)
{
    getSuggestedLib('qeML') 
  
    # args type checking
    if (!is.data.frame(data)) {
        stop("data must be a dataframe or equivalent")
    }

    y <- data[[yName]]
    
    dichotY <- inherits(y, "factor") && length(levels(y) == 2)

    if (!inherits(y, "numeric") &&
        !inherits(y, "integer") &&
        !dichotY
       ) {
        stop("yName must refer to a numeric or 2-level factor column in data.")
    }
    if (!is.factor(data[[sName]])) {
        stop("sName must refer to a factor column in data.")
    }
    if (!is.numeric(data[[xName]])) {
        stop("xName must refer to a numeric column in data.")
    }

    # data engineering #
    # restrict data to fit conditions
    if (is.null(condits)) condits <- '1 > 0'
    if (length(condits) > 1) {
        # combine conditions
        condits <- paste(condits, collapse = " & ")
    }
    restrictions <- sprintf("focusedData <- subset(data, %s)", condits)
    eval(parse(text = restrictions))
    focusedData <- focusedData[c(yName, xName, sName)]
    sCol <- which(names(focusedData) == sName)

    # group the data by S level & execute min size condition
    s <- focusedData[[sName]]
    groupByS <- split(focusedData, s)
    sizes <- sapply(groupByS, nrow)
    tiny <- which(sizes < minS)

    # remove too-small groups
    if (length(tiny) > 0) {
        groupByS <- groupByS[-tiny]
    }

    # consider only the remaining S-levels
    sLevels <- names(groupByS)
    remainingS <- length(sLevels)


   # prepare to plot each sensitive level against X; in this loop, fit
   # the models, and then plot in the following loop
   curXDataList <- list()
   predsList <- list()
   for (i in 1:remainingS) {

       # setup data for training
       curData <- groupByS[[i]][,-sCol]  # current s-level w/o sensitive column
       curXData <- unique(curData[[xName]]) # only the numeric x column
       curXDF <- as.data.frame(curXData)
       names(curXDF) <- xName  # adjust column name

       # fit ML model
       model <- qeFtn(curData, yName, holdout = NULL)
       preds <- predict(model, curXDF)
       if (dichotY) preds <- preds$probs

       # sort data so that lines() will make sense
       curXData <- as.vector(curXData)
       preds <- as.vector(preds)
       orderedXData <- order(curXData)
       curXData <- curXData[orderedXData]
       preds <- preds[orderedXData]

       # store dataframe w/ sorted data for plotting
       # check Loess
       plotdf <- data.frame(curXData, preds)
       if (useLoess) {
           preds <- loess(preds ~ curXData, plotdf)$fitted # loess smoothing
       }

       # these 2 will be used in call to lines()
       curXDataList[[i]] <- curXData
       predsList[[i]] <- preds
   }

   # create plot
   colors <- rainbow(remainingS)
   predsMax <- max(sapply(predsList, max))
   predsMin <- min(sapply(predsList, min))
   ylow <- if (predsMin >= 0) 0.9 * predsMin else 1.1 * predsMin
   yhigh <- if (predsMax >= 0) 1.1 * predsMax else 0.9 * predsMax
   currXMax <- max(sapply(curXData, max))
   currXMin <- min(sapply(curXData, min))

   plot(
        NULL,
        ylim = c(ylow, yhigh),
        xlim = c(currXMin, currXMax),
        xlab = xName,
        ylab = yName,
        main = paste("Underlying Effects of ", sName, " on ", 
            yName, " wrt ", xName)
   )

   for (i in 1:remainingS) {
      lines(
        curXDataList[[i]],
        predsList[[i]],
        type = "l",
        lty = "solid",
        col = colors[i]
      )
   }

   legend(
       x = "topright",
       lty = rep(1, remainingS),
       text.font = 4,
       col = colors,
       text.col = "black",
       legend = sLevels
   )
}

