#' @examples 
#' library(dsld)
#' data(svcensus)
#' dsldConfounders(svcensus, "educ")


# ----------------------- Dispatching w/ Confounders ----------------------- #
#' ::: Description :::
#' @brief description goes here...
#'
#' ::: Arguments :::
#' @param data: dataset, an R dataframe
#' @param sName: name of the sensitive variable, an R factor
#' @param fill: whether or not to fill curve space, an R logical; defaults to
#'      FALSE, not applicable to categorical data
#'
dsldConfounders <- function(data, sName = NULL, fill = FALSE) {
    # dispatch to appropriate auxiliary method
    for (i in 1:ncol(data)) {
        # if categorical
        if (is.factor(data[, i])) {
            print(dsldFrequencyByS(data, colnames(data)[i], sName))
        # if numeric
        } else if (is.numeric(data[, i])) {
            print(dsldDensityByS(data, colnames(data)[i], sName, fill))
        # throw error
        } else {
            stop("Neither categorical or numeric column, check dataframe")
        }
    }
}


# ----------------------- Auxiliary for Numeric ----------------------- #
#' @examples
#' library(dsld)
#' data(svcensus)
#' dsldDensityByS(svcensus)
#'
#' ::: Description :::
#' @brief Graphs densities of a response variable, grouped by a sensitive
#'      variable
#'
#' ::: Arguments :::
#' @param data: A dataframe with 1 numerical column and a factor column
#' @param yName: A name or index of the numerical column
#' @param sName: A name or index of the factor column
#' @param fill: A logical value determining if the graphed curve should be
#'      filled in
#'
#' @export
#'
dsldDensityByS <- function(data, yName = NULL, sName = NULL, fill = FALSE) {
    if (is.null(sName))
        sName <- makeSName(data)
    else if (!class(data[, sName]) %in% c("factor", "character"))
        stop(
            "sName should be of factor or character data type. Consider setting this as a yName instead"
        )
    
    # for now, if theres no sName, this makes one so the function doesnt break
    if (is.null(sName)) {
        Group <- as.factor(rep(1, length(data[, 1])))
        data <- cbind(data, Group)
        sName <- length(data)
    }
    
    # yNames <- a vector of 2 ints/strings that correspond to the columns to be used for
    # the 2 axis on the graph. The user can specify the cols or
    # yNames will be the first 2 columns that are of numeric or integer data type
    if (is.null(yName))
        yName <- makeYNames(data, 1)
    
    sGroups <- levels(unique(data[, sName]))
    
    yNameStr <- names(data[yName])
    sNameStr <- names(data[sName])
    
    filltype = 'none'
    if (fill) filltype = 'tozeroy'
    
    fig <-
      plotly::plot_ly(
        type = 'scatter',
        mode = 'lines',
        fill = filltype
      )
    
    for (i in 1:length(sGroups)) {
        den <- density(data[data[, sName] == sGroups[i],][, yName])
    
        fig <-
            plotly::add_trace(
                fig,
                x = den$x,
                y = den$y,
                mode = 'lines',
                name = sGroups[i]
            )
    }
    
    fig <- plotly::layout(
        fig,
        title = paste("Density of", yNameStr, "by", sNameStr),
        xaxis = list(title = yNameStr),
        yaxis = list(title = "Density"),
        legend = list(title = list(text = sNameStr))
    )
    
    fig       
}


# ----------------------- Auxiliary for Categorical ----------------------- #
#' @examples
#' library(dsld)
#' data(svcensus)
#' dsldFrequencyByS(svcensus)
#'
#' ::: Description :::
#' @brief Extracts frequencies of a response variable, grouped by a sensitive
#'      variable
#'
#' ::: Arguments :::
#' @param data: A dataframe with 1 numerical column and a factor column
#' @param yName: A name or index of the categorical column
#' @param sName: A name or index of the factor column
#'
#' @export
#'
dsldFrequencyByS <- function(data, yName = NULL, sName = NULL) {
    comment <- "
    # ensure libraries #
    getSuggestedLib('gt')

    frequencies <- data %>%
        group_by(sName) %>%
        summarize(Frequency = n()) %>%
        gt()
    "

    # force missing vars #
    # check sensitive variable is missing
    if (is.null(sName)) {
        sName <- makeSName(data)
    # check sensitive variable type
    } else if (!class(data[, sName]) %in% c("factor", "character")) {
        stop(paste(
            "sName should be of factor or character data type. Consider",
            " setting this as a yName instead"
        ))
    }
    
    # check missing response variable
    if (is.null(yName)) {
        yName <- makeYNames(data, 1)
    }


    # -------- Iterative Approach I -------- #
    # # sensitive variable frequencies #
    # # divide by level
    # yGroups <- levels(factor(data[[yName]]))
    # sGroups <- levels(factor(data[[sName]]))

    # # setup dataframe
    # frequencies <- data.frame(matrix(NA, nrow = length(sGroups),
    #     ncol = length(yGroups)))
    # rownames(frequencies) <- sGroups
    # colnames(frequencies) <- yGroups
    
    # # find frequencies for each level
    # for (s in seq_along(sGroups)) {
    #     # for each response level
    #     for (y in seq_along(yGroups)) {
    #         # add frequency
    #         freq <- sum(data[[sName]] == sGroups[s] &&
    #             data[[yName]] == yGroups[y])
    #         frequencies[s, y] <- freq
    #     }
    # }

    # # return frequencies #
    # return(frequencies)


    # -------- Efficient Approach II -------- #
    # sensitive variable frequencies #
    # unique levels to ensure order
    yGroups <- unique(data[[yName]])
    sGroups <- unique(data[[sName]])

    # get a lookup for every s level against every ylevel
    freqLookup <- table(df[[sName]], df[[yName]])

    # convert to dataframe
    frequencies <- as.data.frame.matrix(freqLookup)
    names(frequencies) <- c(
        paste(sName, " Levels"),
        paste0("Frequency of ", unique(data[[yName]]))
    )

    # merge levels of sName into one row


    # return frequncies #
    return(frequencies)
}

library(dsld)
data(svcensus)
dsldFrequencyByS(svcensus, yName = "educ", sName = "gender")


