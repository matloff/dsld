dsldScatterPlot3D <- function (data, axiscols, grpcol, grpVars)
{
  # 3 most occurring group members
  # create an array of dataframes separating each group member
  # for each of those dataframes, add the 3 other columns to a scatterplot
  # scatterplot3d(as.numeric(unlist(mlb1[2])), as.numeric(unlist(mlb1[3])), as.numeric(unlist(mlb1[4])), color="Blue")
  
  data1 <- data[data[grpcol] == grpVars[1],]
  data2 <- data[data[grpcol] == grpVars[2],]
  
  # 
  # data1axis <- as.numeric(unlist(data1[2]))
  # data1axis <- rbind(data1axis, as.numeric(unlist(data1[3])))
  # data1axis <- rbind(data1axis, as.numeric(unlist(data1[4])))

  # print(data1axis)

  data1axis <- as.numeric(unlist(data1[axiscols[1]]))
  
  for (i in c(axiscols[2:3]))
  {
    data1axis <- rbind(data1axis, as.numeric(unlist(data1[i])))
  }

  data2axis <- as.numeric(unlist(data1[axiscols[1]]))
  
  for (i in c(axiscols[2:3]))
  {
    data2axis <- rbind(data2axis, as.numeric(unlist(data2[i])))
  }
  

  sp <- scatterplot3d::scatterplot3d(data1axis[1,], data1axis[2,], data1axis[3,], color = "Red", angle = 45)
  sp$points3d(data2axis[1,], data2axis[2,], data2axis[3,], color = "Blue")
  
}
dsldScatterPlot3D(mlb1, 2:4, 1, c("Catcher", "Outfielder"))