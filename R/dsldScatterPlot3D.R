# TODO
# documentation
# gets the group names (catcher, outfielder, etc) automatically (Completed?)
data("mlb1")
dsldScatterPlot3D <- function (data, axiscols, grpcol, angle, grpnames=NULL, sortedby="Name",
                               numgrps=NULL, main=NULL, sub=NULL, xlim=NULL,
                               ylim=NULL, zlim=NULL)
{
  group <- switch(
    sortedby,
    "Name" = groups <- levels(unique(data[,grpcol])),
    "Frequency" = groups <- names(sort(table(data[,grpcol]),decreasing=T)),
    "Frequnecy-Descending" = groups <- names(sort(table(data[,grpcol]),decreasing=F))
  )
  
  if (!missing(grpnames)) groups <- grpnames
  else
  {
    groups <- groups[1:numgrps]
  }
  
  legend_labels <- groups
  legend_col <- 1
  #Creates a subset of the data set for each group
  for(i in 1:length(groups))
  {
    group_name <- paste("data", i, sep = "")
    group <- data[data[grpcol] == groups[i],][axiscols]
    assign(group_name, group)

    #Initializes a scatter plot with the first data
    if (i == 1)
    { sp <- scatterplot3d::scatterplot3d(data1[,1], data1[,2], data1[,3], color = 1, angle = angle,
                          xlab = names(data[axiscols[1]]),
                          ylab = names(data[axiscols[2]]),
                          zlab = names(data[axiscols[3]]),
                          main = main,
                          sub = sub,
                          xlim = xlim,
                          ylim = ylim,
                          zlim = zlim)
    }
    else {
      #Plots each group as a different color
      sp$points3d(eval(parse(text=group_name))[,1],
                  eval(parse(text=group_name))[,2],
                  eval(parse(text=group_name))[,3], col = i)
      legend_col <- c(legend_col, i)
    }
    
  }
  
  legend("topright", inset=c(-0.05,-0.05), legend=legend_labels, col=legend_col, pch = 1, xpd = TRUE)
}
library(qeML)
data(mlb1)
dsldScatterPlot3D(mlb1, 2:4, 1, 30, numgrps = 3, sortedby="Frequency")
