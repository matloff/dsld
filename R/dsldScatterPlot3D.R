dsldScatterPlot3D <-
  function(data,
           yNames,
           sName,
           sGroups = NULL,
           sortedBy = "Name",
           numGroups = 8,
           maxPoints = NULL,
           xlim = NULL,
           ylim = NULL,
           zlim = NULL,
           main = NULL,
           colors = "Paired",
           opacity = 1,
           pointSize = 8) {
    getSuggestedLib("plotly")
    
    # Limit amount of data points
    if (!is.null(maxPoints))
      data <- data[1:maxPoints, ]
    
    if (!class(data[, sName]) %in% c("factor", "character"))
      stop(
        "sName should be of factor or character data type. 
        Consider setting this as yName instead"
      )
    
    if (length(yNames) != 3)
      stop("ScatterPlot3d requires 3 variables for the 3 axis")
    
    # sGroups <- a vector of the individual group names in the 'data'.
    # the user can supply sGroups as an vector of names they want to look at
    if (is.null(sGroups))
      sGroups <- makeSGroups(data, sName, numGroups, sortedBy)
    
    # limits dataset to include only those with a group in groupNames
    data <- data[data[, sName] %in% sGroups, ]
    data <- droplevels(data)
    
    # Limit values of data points
    if (!is.null(xlim) | !is.null(ylim) | !is.null(zlim))
      data <- limitRange(data, yNames, xlim, ylim, zlim)
    
    # Creates a title
    if (is.null(main)) {
      for (yName in names(data[yNames]))
        main <- paste(main, yName)
      main <- paste(main, " by ", names(data[sName]))
    }
    
    # save this to print to the text of each point
    original <- data
    # numeric for a cleaner looking graph if the axis is factor type
    data[, yNames] <- sapply(data[, yNames], as.numeric)
    # info card for each data point
    text <- paste("<extra></extra>", sep = "")
    for (i in 1:length(data))
      text <-
      paste(text, names(data[i]), ": ", original[, i], "<br>", sep = "")
    
    # plotting the points
    fig <- plotly::plot_ly(
      data,
      x = data[, yNames[1]],
      y = data[, yNames[2]],
      z = data[, yNames[3]],
      color = data[, sName],
      colors = colors,
      hovertemplate = text,
      marker = list(size = pointSize,
                    opacity = opacity)
    )
    fig <- plotly::add_markers(fig)
    # add labels and axis
    fig <- plotly::layout(
      fig,
      title = main,
      scene = list(
        xaxis = list(title = paste(names(data[yNames[1]]), "(X)")),
        yaxis = list(title = paste(names(data[yNames[2]]), "(Y)")),
        zaxis = list(title = paste(names(data[yNames[3]]), "(Z)")),
        legend = list(title = list(text = names(data[sName])))
      )
    )
    
    fig
  }

# ---- Test Cases ----
# library(dsld)
# data(svcensus)
# dsldScatterPlot3D(svcensus, yNames = c("educ", "wageinc", "occ"), sName = "gender")
