# TODO
# documentation
# gets the group names (catcher, outfielder, etc) automatically (Completed?)
# create the axes with a for loop
data("mlb1")
dsldScatterPlot3D <- function (data, axiscols, grpcol, angle)
{

  groups <- unique(data[,grpcol]) #Separates column into groups (i.e catcher, outfielder, shortstop, etc)

  #Creates a subset of the data set for each group
  for(i in 1:length(groups))
  {
    group_name <- paste("data", i, sep = "")
    group <- data[data[grpcol] == levels(groups)[i],][axiscols]
    assign(group_name, group)

    #Initializes a scatter plot with the first data
    if (i == 1)
    { sp <- scatterplot3d(data1[,1], data1[,2], data1[,3], color= "red", angle = angle,
                          xlab = names(data[axiscols[1]]),
                          ylab = names(data[axiscols[2]]),
                          zlab = names(data[axiscols[3]]))
    }

    #Plots each group as a different color
    sp$points3d(eval(parse(text=group_name))[,1],
                eval(parse(text=group_name))[,2],
                eval(parse(text=group_name))[,3], col = i*20)
  }
}
dsldScatterPlot3D(mlb1, 2:4, 1,10)

