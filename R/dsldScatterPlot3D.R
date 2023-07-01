
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
    group <- as.factor(rep(1,length(data[,1])))
    data <- cbind(data, group)
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
                           opacity = opacity))
  fig <- plotly::add_markers(fig)
  fig <- plotly::layout(fig, 
                        title = main,
                        scene = list(xaxis = list(title = names(data[yNames[1]])),
                                     yaxis = list(title = names(data[yNames[2]])),
                                     zaxis = list(title = names(data[yNames[3]]))),
                        legend = list(title = list(text = names(data[sName]))))
  
  fig
}

# ---- scatterplot3d ----

# dsldScatterPlot3D <- function (data, grpcol=NULL, axiscols=NULL, grpnames=NULL, angle=40,  
#                                sortedby="Frequency", numgrps=3, 
#                                colors=NULL, pchs=NULL, main=NULL, sub=NULL, 
#                                xlim=NULL, ylim=NULL, zlim=NULL) {
#   getSuggestedLib("scatterplot3d")
#   
#   data_types <- sapply(data, class) # the datatypes of each column in data
#   
#   # grpcol <- an int/string of the col of the grouping variable. 
#   # the variable the determines the colors of the dots. user can specify or
#   # grpcol will be the col with the lowest amount of unique values
#   if (missing(grpcol)) {
#     num_uniques <- sort(sapply(sapply(data, unique), length)) 
#     # how many distinct values for each column, sorted by least unique values
#     for (i in 1:length(data_types)) {
#       col <- data_types[names(num_uniques[i])]
#       if (col == "factor" || col == "character"){
#         grpcol <- names(col)
#         break
#       }
#     }
#   } else {
#     if (data_types[grpcol] != "factor" && data_types[grpcol] != "character")
#       stop("grpcol should be of factor or character data type. Consider setting this as an axiscol instead")
#   }
#   
#   # axiscols <- a vector of 3 ints/strings that correspond to the columns to be used for
#   # the 3 axis on the graph. The user can specify the cols or
#   # axiscols will be the first 3 columns that are of numeric or integer data type
#   if (missing(axiscols)) {
#     axiscols <- vector()
#     for (i in 1:length(data_types)) {
#       if (data_types[i] == "numeric" || data_types[i] == "integer") {
#         axiscols <- c(axiscols, i)
#       }
#       if (length(axiscols) == 3) break
#     }
#   }
#   if (length(axiscols) != 3) stop("ScatterPlot3d requires 3 variables for the 3 axis")
#   
#   
#   # grpnames <- a vector of the individual group names in the 'data'. 
#   # the user can supply grpnames as an vector of names they want to look at
#   if (missing(grpnames) && !missing(grpcol)) {
#     # If there are 8 possible types the group variable can be, the vector is 8 long. 
#     # Sorted according to user
#     switch(
#       sortedby,
#       "Name" = grpnames <- levels(unique(data[,grpcol])),
#       "Frequency" = grpnames <- names(sort(table(data[,grpcol]),decreasing=T)),
#       "Frequency-Descending" = grpnames <- names(sort(table(data[,grpcol]),decreasing=F))
#     )
#     # otherwise the vector is cut off to only have numgrps number of grpnames
#     if (length(grpnames) > numgrps) grpnames <- grpnames[1:numgrps]
#   }
#   numgrps <- length(grpnames)
#   
#   # Colors and symbols displayed on the graph
#   if (missing(colors)) colors <- 1:numgrps + 1
#   else colors <- rep(colors, numgrps) # add enough colors to be displayed if not enough
#   if (missing(pchs)) pchs <- 1:numgrps 
#   else pchs <- rep(pchs, numgrps) # add enough symbols to be displayed if not enough
#   
#   # Title of the graph
#   if (missing(main) && !missing(grpcol)) {
#     main <- paste(names(data[grpcol]), " vs. ", 
#                   names(data[axiscols[1]]), ", ", 
#                   names(data[axiscols[2]]), ", ", 
#                   names(data[axiscols[3]]))
#   }
#   
#   for(i in 1:numgrps) {
#     # group_name <- a new variable that holds every point belonging to a specific group name
#     # for example, a new variable data1 could contain the 3 columns of only catchers
#     group_name <- paste("data", i, sep = "")
#     group <- data[data[grpcol] == grpnames[i],][axiscols]
#     assign(group_name, group)
# 
#     
#     if (i == 1) { 
#       # makes a normal scatterplot if there are no groups
#       if (missing(grpcol)) data1 <- data[axiscols]
#       
#       # Initializes a scatter plot with the first data
#       sp <- scatterplot3d::scatterplot3d(data1[,1], data1[,2], data1[,3], 
#                           color = colors[1], 
#                           pch = pchs[1],
#                           angle = angle,
#                           xlab = names(data[axiscols[1]]),
#                           ylab = names(data[axiscols[2]]),
#                           zlab = names(data[axiscols[3]]),
#                           main = main,
#                           sub = sub,
#                           xlim = xlim,
#                           ylim = ylim,
#                           zlim = zlim)
#     } else {
#       # Adds points to the already existing scatterplot, sp
#       sp$points3d(eval(parse(text=group_name))[,1],
#                   eval(parse(text=group_name))[,2],
#                   eval(parse(text=group_name))[,3], 
#                   col = colors[i], 
#                   pch = pchs[i])
#     }
#     
#   }
#   
#   # creates the legend if there are more than 1 groups
#   legend_side <- "bottomright"
#   if (angle < 0 || angle > 180) legend_side <- "bottomleft"
#   if (!missing(grpcol)) legend(legend_side, inset=c(0, -0.3), title=names(data[grpcol]),
#          legend=grpnames, col=colors, pch = pchs, xpd = TRUE)
# }

# ---- Test Cases ----
# library(dsld)
# data(pef)
# dsldScatterPlot3D(pef, axiscols = c("educ", "wageinc", "occ"))

# library(qeML)
# data(mlb)
# dsldScatterPlot3D(mlb)

