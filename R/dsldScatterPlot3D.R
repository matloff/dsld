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
  data1 <- data[data[grpcol] == grpVars[1],]
  
  # we need to convert a list into a float for the scatterplot function
  # so this converts our dataframe from a list to a float i think
  # ideally theres a function that does an operation on each row of a matrix but i forgot
  data1axis <- as.numeric(unlist(data1[axiscols[1]]))
  for (i in c(axiscols[2:3]))
  {
    data1axis <- rbind(data1axis, as.numeric(unlist(data1[i])))
  }

  data2 <- data[data[grpcol] == grpVars[2],]
  data2axis <- as.numeric(unlist(data1[axiscols[1]]))
  for (i in c(axiscols[2:3]))
  {
    data2axis <- rbind(data2axis, as.numeric(unlist(data2[i])))
  }
  
  sp <- scatterplot3d::scatterplot3d(data1axis[1,], data1axis[2,], data1axis[3,], col= "red", angle = 45)
  sp$points3d(data2axis[1,], data2axis[2,], data2axis[3,], col = "blue")
  
}
dsldScatterPlot3D(mlb1, 2:4, 1, c("Catcher", "Outfielder"))
