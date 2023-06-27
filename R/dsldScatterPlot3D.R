# TODO
# documentation
# gets the group names (catcher, outfielder, etc) automatically (Completed?)
data("mlb1")
dsldScatterPlot3D <- function (data, axiscols, grpcol, angle, grpnames=NULL, 
                               sortedby="Name",
                               numgrps=3, main=NULL, sub=NULL, xlim=NULL,
                               ylim=NULL, zlim=NULL) {
  # groups <- a vector of the individual groups in the 'data'. If there are 8 possible
  # types the group variable can be, the vector is 8 long. Sorted according to user
  switch(
    sortedby,
    "Name" = groups <- levels(unique(data[,grpcol])),
    "Frequency" = groups <- names(sort(table(data[,grpcol]),decreasing=T)),
    "Frequnecy-Descending" = groups <- names(sort(table(data[,grpcol]),decreasing=F))
  )
  
  # the user can supply groups as an vector of names they want to look at
  if (!missing(grpnames)) groups <- grpnames
  # otherwise the vector is cut off to only have numgrps number of groups
  else groups <- groups[1:numgrps]
  
  # The legend that displays what each circle color means
  legend_labels <- groups
  legend_col <- 1
  
  for(i in 1:length(groups)) {
    # group_name <- a new variable that holds every point belonging to a specific group name
    # for example, a new variable data1 could contain the 3 columns of only catchers
    group_name <- paste("data", i, sep = "")
    group <- data[data[grpcol] == groups[i],][axiscols]
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
dsldScatterPlot3D(mlb1, 2:4, 1, 30, numgrps = 3, sortedby="Frequency")

