### --------------------------- dsldGetRow5 ------------------------------------
### selects 5 rows for comparison across each level of the sensitive variable
### randomly if the user doesn't supply data in the interactions case
### USEFUL FOR: dsldLinear, dsldLogit
dsldGetRow5 <- function(data, yName, sName) {
  rows <- sample(nrow(data), 5)
  reducedData <- data[rows, ]
  columns <- c(yName, sName)
  newDat <- reducedData[, !(names(reducedData) %in% columns)]
  result <- sprintf("No user sComparisonPts supplied. The following rows 
                    are selected: %s,%s,%s,%s,%s", rows[1],rows[2],rows[3],rows[4],
                    rows[5]); print(result)
  return(newDat)
}
