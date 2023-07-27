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
dsldConfounders <- function(data, sName = NULL) {
    # dispatch to appropriate auxiliary method
    for (i in 1:seq_len(ncol(data))) {
        # if categorical
        if (is.factor(data[, i])) {
            print(dsldFrequencyByS(data, colnames(data)[i], sName))
            cat("Press <ENTER> to view next density graph / frequency dataframe...")
            temp_input <- readline()
        # if numeric
        } else if (is.numeric(data[, i])) {
            print(dsldDensityByS(data, colnames(data)[i], sName))
            cat("Press <ENTER> to view next density graph / frequency dataframe...")
            temp_input <- readline()
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
#' dsld::dsldDensityByS(svcensus, "wageinc", "educ")
#'
#' ::: Description :::
#' @brief Graphs densities of a response variable, grouped by a sensitive
#'      variable
#'
#' ::: Arguments :::
#' @param data: A dataframe with 1 numerical column and a factor column
#' @param yName: A name or index of the numerical column
#' @param sName: A name or index of the factor column
#'
#' @export
#'
dsldDensityByS <- function(data, yName = NULL, sName = NULL) {
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
  
  numGroups <- length(levels(unique(data[, sName])))
  
  yNameStr <- names(data[yName])
  sNameStr <- names(data[sName])
  
  bw <- seq(.25,4,.25)
  
  # aval <- a list of the arguements of all the lines we're going to graph
  aval <- list()
  for (i in 1:length(bw)) {
    # from plotly: creating a single group-separated density dataframe object to graph
    dens <-
      with(data, tapply(data[, yName], INDEX = data[, sName], density, adjust = bw[i]))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      group = rep(names(dens), each = length(dens[[1]]$x))
    )
    
    aval[[i]] <- list(
      visible = FALSE,
      x =  df$x,
      y =  df$y
    )
  }
  
  aval[[4]]$visible = TRUE
  
  steps <- list()
  fig <- plotly::plot_ly(type = 'scatter',
                         mode = 'lines',
                         color = df$group)
  for (i in 1:length(bw)) {
    fig <-
      plotly::add_lines(
        fig,
        x = aval[[i]]$x,
        y = aval[[i]]$y,
        visible = aval[[i]]$visible
      )
    # if there are 3 groups in sName, and there are 8 bandwidths, we need to initally
    # set all 24 graphs's visibility to false
    step <- list(args = list('visible', rep(FALSE, length(aval) * numGroups)),
                 method = 'restyle', label = bw[i])
    # and then the correct 3 to true
    step$args[[2]][1:numGroups + numGroups * i] <- TRUE
    steps[[i]] <- step
  }
  
  buttons <- list(
    list(method="restyle", args= list("fill", "none"), label="no fill"),
    list(method="restyle", args= list("fill", "tozeroy"), label="fill")
  )
  
  # add slider control to plot
  fig <-
    plotly::layout(
      fig,
      updatemenus = list(list(active = 0, x = 0, y = 1, 
                              buttons=buttons)),
      sliders = list(list(
        active = 3,
        currentvalue = list(prefix = "Adjust: "),
        steps = steps
      )),
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
    freqLookup <- table(data[[sName]], data[[yName]])
    
    # convert to dataframe
    frequencies <- as.data.frame.matrix(freqLookup)
    sNameStr <- paste(sName, " Levels")
    names(frequencies) <- c(
        # sNameStr,
        paste0("Frequency of ", yGroups)
    )

    # # merge levels of sName into one row
    # uniqueSGroups <- data.frame(sName = unique(data[[sName]]))
    # names(uniqueSGroups) <- c(sNameStr)
    # frequencies <- merge(uniqueSGroups, frequencies, by = sNameStr,
    #     all.x = TRUE)

    # # fill missing combination values with 0
    # frequencies[is.na(frequencies)] <- 0

    # return frequncies #
    return(frequencies)
}

# library(dsld)
# data(svcensus)
# dsldFrequencyByS(svcensus, yName = "educ", sName = "gender")


