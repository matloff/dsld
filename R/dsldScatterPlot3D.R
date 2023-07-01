# TODO
# make sName work with numeric (use a heat map)
# make circles hollow


# ---- plotly ----

dsldScatterPlot3D <- function(data, sName=NULL, yNames=NULL, sGroups=NULL, 
                              sortedBy="Name", numGroups=8, maxPoints=NULL, 
                              main=NULL, colors="Paired", opacity=1, 
                              pointSize=8) {
  dsld::getSuggestedLib("plotly")
  
  if (!missing(maxPoints))
    data <- data[1:maxPoints,]
  
  data_types <- sapply(data, class) # the datatypes of each column in data

  # sName <- an int/string of the col of the grouping variable.
  # the variable the determines the colors of the dots. user can specify or
  # sName will be the col with the lowest amount of unique values
  if (missing(sName)) {
    num_uniques <- sort(sapply(sapply(data, unique), length))
    # how many distinct values for each column, sorted by least unique values
    for (i in 1:length(data_types)) {
      col <- data_types[names(num_uniques[i])]
      if (col %in% c("factor", "character")){
        sName <- names(col)
        break
      }
    }
  } else {
    if (!data_types[sName] %in% c("factor", "character"))
      stop("sName should be of factor or character data type. Consider setting this as an axiscol instead")
  }
  # for now, if theres no sName, this makes one so the function doesnt break
  if (missing(sName)) {
    Group <- as.factor(rep(1,length(data[,1])))
    data <- cbind(data, Group)
    sName <- length(data)
  }

  # yNames <- a vector of 3 ints/strings that correspond to the columns to be used for
  # the 3 axis on the graph. The user can specify the cols or
  # yNames will be the first 3 columns that are of numeric or integer data type
  if (missing(yNames)) {
    yNames <- vector()
    for (i in 1:length(data_types)) {
      if (data_types[i] %in% c("integer", "numeric")) {
        yNames <- c(yNames, i)
      }
      if (length(yNames) == 3) break
    }
  }
  if (length(yNames) != 3) stop("ScatterPlot3d requires 3 variables for the 3 axis")
  
  # sGroups <- a vector of the individual group names in the 'data'.
  # the user can supply sGroups as an vector of names they want to look at
  if (missing(sGroups) && !missing(sName)) {
    # If there are 8 possible types the group variable can be, the vector is 8 long.
    # Sorted according to user
    switch(
      sortedBy,
      "Name" = sGroups <- levels(unique(data[,sName])),
      "Frequency" = sGroups <- names(sort(table(data[,sName]),decreasing=T)),
      "Frequency-Descending" = sGroups <- names(sort(table(data[,sName]),decreasing=F))
    )
    # otherwise the vector is cut off to only have numGroups number of sGroups
    if (length(sGroups) > numGroups) sGroups <- sGroups[1:numGroups]
  }
  # limits dataset to include only those with a group in groupNames
  data <- data[data[,sName] %in% sGroups,]
  data <- droplevels(data)
  
  # Creates a title
  if (missing(main) && !missing(sName)) {
    main <- paste(names(data[sName]), " vs. ")
    for (yName in names(data[yNames]))
      main <- paste(main, yName)
  }
  
  # save this to print to the text of each point
  original <- data
  # numeric for a cleaner looking graph if the axis is factor type
  data[,yNames] <- sapply(data[,yNames], as.numeric)
  
  # info card for each data point
  text <- paste("<extra></extra>", sep="")
  for (i in 1:length(data)) 
    text <- paste(text, names(data[i]), ": ", original[,i], "<br>", sep="")
   
  
  fig <- plotly::plot_ly(data, 
                         x = data[,yNames[1]], 
                         y = data[,yNames[2]], 
                         z = data[,yNames[3]], 
                         color = data[,sName], 
                         colors = colors,
                         hovertemplate = text,
                         marker = list(
                           size = pointSize,
                           opacity = 0.5,
                           line = list(
                            color = colors,
                            opacity = 1
                            )
                          )
                        )
  fig <- plotly::add_markers(fig)
  fig <- plotly::layout(fig, 
                        title = main,
                        scene = list(xaxis = list(title = names(data[yNames[1]])),
                                     yaxis = list(title = names(data[yNames[2]])),
                                     zaxis = list(title = names(data[yNames[3]]))),
                        legend = list(title = list(text = names(data[sName]))))
  
  fig
}

# ---- Test Cases ----
# library(dsld)
# data(pef)
# dsldScatterPlot3D(pef, yNames = c("educ", "wageinc", "occ"))

# library(qeML)
# data(mlb)
# dsldScatterPlot3D(mlb)

