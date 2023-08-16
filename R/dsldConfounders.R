#' @examples 
#' library(dsld)
#' data(svcensus)
#' dsldConfounders(svcensus, "educ")


# ----------------------- Dispatching w/ Confounders ----------------------- #
#' ::: Description :::
#' @brief a dispatching function that allows users to pass in a dataframe they
#'      want to analyze in the context of finding confounders for numeric and
#'      categorical variables. Dispatches frequency by sensitive var or density
#'      by sensitive var against every column in the dataframe as appropriate.
#'
#' ::: Arguments :::
#' @param data: dataset, an R dataframe
#' @param sName: name of the sensitive variable, an R factor
#' @param graphType: type of graph to display, an R character; defaults to
#'      "plotly", but can be one of ("plotly", "plot")
#' @param fill: whether or not to fill curve space, an R logical; defaults to
#'      FALSE, not applicable to categorical data
#'
dsldConfounders <- function(data, sName, graphType = "plotly", fill = FALSE) {
    # Error checking
    if (is.null(sName)) {
        stop(paste("sName must be provided as a string of a column name"))
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
                temp_input <- readline()
            }
        # if numeric
        } else if (is.numeric(data[, i])) {
            print(dsldDensityByS(data, colnames(data)[i], sName, graphType, fill))

            # require input if there's a next
            if (i != numCols) {
                cat("Press <ENTER> to view next density graph / frequency dataframe...\n")
                temp_input <- readline()
            }
        # throw error
        } else {
            stop(paste("Neither categorical or numeric column, check dataframe"))
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
#' @param cName: A name or index of the numerical column
#' @param sName: A name or index of the factor column
#'
#' @export
#'
dsldDensityByS <- function(data, cName, sName, graphType = "plotly", fill = FALSE) {
    if (!class(data[, sName]) %in% c("factor", "character"))
        stop(paste("sName should be of factor or character data type. Consider setting this as a cName instead"))

    numGroups <- length(levels(unique(data[, sName])))
    
    cNameStr <- names(data[cName])
    sNameStr <- names(data[sName])
    
    bw <- seq(.25, 4, .25)
    
    if (tolower(graphType) == "plot") {
        getSuggestedLib('ggplot2')
        # ************ plot() *********************************
        sGroups <- levels(unique(data[, sName]))
        for (i in 1:length(sGroups)) {
            den <- density(data[data[, sName] == sGroups[i], ][, cName])

            if (i == 1)
                plot(
                    den,
                    col = i,
                    xlab = cNameStr,
                    main = paste("Density of", cNameStr, "by", sNameStr)
                )
            else
                lines(den, col = i)

            if (fill)
                polygon(den, col = i)
        }

        legend(
            "topright",
            title = sNameStr,
            legend = sGroups,
            col = 1:length(sGroups),
            lty = 1
        )
        # ************ plot() *********************************
    } else if (tolower(graphType) == "plotly") {
        getSuggestedLib('plotly')
        # ************ plotly *********************************
        # aval <- a list of the arguements of all the lines we're going to graph
        aval <- list()
        for (i in 1:length(bw)) {
            # from plotly: creating a single group-separated density dataframe object to graph
            dens <-
            with(data, tapply(data[, cName], INDEX = data[, sName], density, adjust = bw[i]))
            df <- data.frame(
            x = unlist(lapply(dens, "[[", "x")),
            y = unlist(lapply(dens, "[[", "y")),
            group = rep(names(dens), each = length(dens[[1]]$x))
            )
            
            aval[[i]] <- list(visible = FALSE,
                            x =  df$x,
                            y =  df$y)
        }
        
        aval[[4]]$visible = TRUE
        
        steps <- list()
        fig <- plotly::plot_ly(type = 'scatter',
                                mode = 'lines',
                                color = df$group)
        for (i in 1:length(bw)) {
            fig <-
            plotly::add_lines(fig,
                                x = aval[[i]]$x,
                                y = aval[[i]]$y,
                                visible = aval[[i]]$visible)
            # if there are 3 groups in sName, and there are 8 bandwidths, we need to initally
            # set all 24 graphs's visibility to false
            step <-
            list(
                args = list('visible', rep(FALSE, length(aval) * numGroups)),
                method = 'restyle',
                label = bw[i]
            )
            # and then the correct 3 to true
            step$args[[2]][1:numGroups + numGroups * i] <- TRUE
            steps[[i]] <- step
        }
        
        buttons <- list(
            list(
            method = "restyle",
            args = list("fill", "none"),
            label = "no fill"
            ),
            list(
            method = "restyle",
            args = list("fill", "tozeroy"),
            label = "fill"
            )
        )
        
        # add slider control to plot
        fig <-
            plotly::layout(
            fig,
            updatemenus = list(list(
                active = 0,
                x = 0,
                y = 1,
                buttons = buttons
            )),
            sliders = list(list(
                active = 3,
                currentvalue = list(prefix = "Adjust: "),
                steps = steps
            )),
            title = paste("Density of", cNameStr, "by", sNameStr),
            xaxis = list(title = cNameStr),
            yaxis = list(title = "Density"),
            legend = list(title = list(text = sNameStr))
            )
        fig
        # ************ plotly *********************************
    }
}

# ----------------------- Auxiliary for Categorical ----------------------- #
#' @examples
#' library(dsld)
#' data(svcensus)
#' dsldFrequencyByS(svcensus, cName = "educ", sName = "gender")
#'
#' ::: Description :::
#' @brief Extracts frequencies of a combination of levels, grouped by a
#'      sensitive variable and the response variable.
#'
#' ::: Arguments :::
#' @param data: A dataframe with 1 numerical column and a factor column
#' @param cName: A name or index of the categorical column
#' @param sName: A name or index of the factor column
#'
#' @export
#'
dsldFrequencyByS <- function(data, cName, sName) {
    comment <- "
    # ensure libraries #
    getSuggestedLib('gt')

    frequencies <- data %>%
        group_by(sName) %>%
        summarize(Frequency = n()) %>%
        gt()
    "

    # type validation #
    if (!class(data[, sName]) %in% c("factor", "character")) {
        stop(paste(
            "sName should be of factor or character data type."
        ))
    }
    if (!class(data[, cName]) %in% c("factor", "character")) {
        stop(paste(
            "cName should be of factor or character data type. Consider",
            " calling `dsldDensityByS(data, cName = ",
            cName,
            ", sName = ",
            sName,
            ")` instead",
            sep = ""
        ))
    }


    # sensitive variable frequencies #
    # unique levels to ensure order
    yGroups <- unique(data[[cName]])

    # get a lookup for every s level against every ylevel
    freqLookup <- table(data[[sName]], data[[cName]])
    
    # convert counts to percentages
    freqLookup <- freqLookup/rowSums(freqLookup)
    
    # convert to dataframe
    frequencies <- as.data.frame.matrix(freqLookup)
    names(frequencies) <- c(
        paste0("Frequency of ", yGroups)
    )

    # return frequncies #
    return(frequencies)
}


