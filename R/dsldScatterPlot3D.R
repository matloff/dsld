# TODO 
# documentation
# gets the group names (catcher, outfielder, etc) automatically
# create the axes with a for loop

dsldScatterPlot3D <- function (data, axiscols, grpcol, grpVars)
{
  # function structure
  # 3 most occurring group members
  # create an array of dataframes separating each group member
  # for each of those dataframes, add the 3 other columns to a scatterplot
  
  # two separate dataframes
  # for example the data of only catchers and the data of only outfielders
  data1 <- data[data[grpcol] == grpVars[1],][axiscols]
  data2 <- data[data[grpcol] == grpVars[2],][axiscols]

  sp <- scatterplot3d::scatterplot3d(data1[,1], data1[,2], data1[,3], color= "red", angle = 45)
  sp$points3d(data2[,1], data2[,2], data2[,3], col = "blue")
  
}
dsldScatterPlot3D(mlb1, 2:4, 1, c("Catcher", "Outfielder"))
