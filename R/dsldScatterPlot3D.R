# TODO
# documentation
# gets the group names (catcher, outfielder, etc) automatically (Completed?)

dsldScatterPlot3D <- function (data, grpcol, axiscols=NULL, grpnames=NULL, angle=40,  
                               sortedby="Name",
                               numgrps=3, main=NULL, sub=NULL, xlim=NULL,
                               ylim=NULL, zlim=NULL) {
  data_types <- sapply(data, class) # the datatypes of each column in data
  
  # grpcol <- an int/string of the col of the grouping variable. 
  # the variable the determines the colors of the dots. user can specify or
  # grpcol will be the col with the lowest amount of unique values
  if (missing(grpcol)) {
    num_uniques <- sort(sapply(sapply(data, unique), length)) 
    # how many distinct values for each column, sorted by least unique values
    for (i in 1:length(data_types)) {
      col <- data_types[names(num_uniques[i])]
      if (col == "factor" || col == "character"){
        grpcol <- names(col)
        break
      }
    }
  }
  
  # axiscols <- a vector of 3 ints/strings that correspond to the columns to be used for
  # the 3 axis on the graph. The user can specify the cols or
  # axiscols will be the first 3 columns that are of numeric or integer data type
  if (missing(axiscols)) {
    axiscols <- vector()
    for (i in 1:length(data_types)) {
      if (data_types[i] == "numeric" || data_types[i] == "integer") {
        axiscols <- c(axiscols, i)
      }
      if (length(axiscols) == 3) break
    }
  }
  
  # grpnames <- a vector of the individual group names in the 'data'. 
  # the user can supply grpnames as an vector of names they want to look at
  if (missing(grpnames)) {
    # If there are 8 possible types the group variable can be, the vector is 8 long. 
    # Sorted according to user
    switch(
      sortedby,
      "Name" = grpnames <- levels(unique(data[,grpcol])),
      "Frequency" = grpnames <- names(sort(table(data[,grpcol]),decreasing=T)),
      "Frequency-Descending" = grpnames <- names(sort(table(data[,grpcol]),decreasing=F))
    )
    # otherwise the vector is cut off to only have numgrps number of grpnames
    grpnames <- grpnames[1:numgrps]
  }
  
  # The legend that displays what each circle color means
  legend_labels <- grpnames
  legend_col <- 1
  
  for(i in 1:length(grpnames)) {
    # group_name <- a new variable that holds every point belonging to a specific group name
    # for example, a new variable data1 could contain the 3 columns of only catchers
    group_name <- paste("data", i, sep = "")
    group <- data[data[grpcol] == grpnames[i],][axiscols]
    assign(group_name, group)

    #Initializes a scatter plot with the first data
    if (i == 1) { 
      sp <- scatterplot3d::scatterplot3d(data1[,1], data1[,2], data1[,3], 
                          color = 1, 
                          angle = angle,
                          xlab = names(data[axiscols[1]]),
                          ylab = names(data[axiscols[2]]),
                          zlab = names(data[axiscols[3]]),
                          main = main,
                          sub = sub,
                          xlim = xlim,
                          ylim = ylim,
                          zlim = zlim)
    } else {
      # Adds points to the already existing scatterplot, sp
      sp$points3d(eval(parse(text=group_name))[,1],
                  eval(parse(text=group_name))[,2],
                  eval(parse(text=group_name))[,3], col = i)
      legend_col <- c(legend_col, i)
    }
    
  }
  
  # creates the legend
  legend("topright", inset=c(-0.05,-0.05), title=names(data[grpcol]),
         legend=legend_labels, col=legend_col, pch = 1, xpd = TRUE)
}
library(qeML)
data(mlb1)
dsldScatterPlot3D(mlb)

