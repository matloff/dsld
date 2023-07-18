# Name pending

dsldConfounders <- function(data, yName=NULL, sName=NULL, fill=FALSE) {
  
  if (is.null(sName)) sName <- makeSName(data)
  else if (!class(data[,sName]) %in% c("factor", "character")) 
    stop("sName should be of factor or character data type. Consider setting this as an axiscol instead")
  
  # for now, if theres no sName, this makes one so the function doesnt break
  if (is.null(sName)) {
    Group <- as.factor(rep(1,length(data[,1])))
    data <- cbind(data, Group)
    sName <- length(data)
  }
  
  # yNames <- a vector of 2 ints/strings that correspond to the columns to be used for
  # the 2 axis on the graph. The user can specify the cols or
  # yNames will be the first 2 columns that are of numeric or integer data type
  if (is.null(yName)) 
    yName <- makeYNames(data, 1)
  
  sGroups <- levels(unique(data[,sName]))
  
  yNameStr <- names(data[yName])
  sNameStr <- names(data[sName])
  
  for (i in 1:length(sGroups)) {
    den <- density(data[data[,sName] == sGroups[i],][,yName])
    
    if (i == 1) plot(den, col = i, 
                     xlab = yNameStr,
                     main = paste("Density of", yNameStr, "vs.", sNameStr))
    else lines(den, col = i)
    
    if (fill) polygon(den, col = i)
  }
  
  legend("topright", title = sNameStr, 
         legend = sGroups, col = 1:length(sGroups), lty = 1)
}

makeSName <- function(data) {
  data_types <- sapply(data, class) # the datatypes of each column in data
  
  num_uniques <- sort(sapply(sapply(data, unique), length))
  sName <- NULL
  # how many distinct values for each column, sorted by least unique values
  for (i in 1:length(data_types)) {
    col <- data_types[names(num_uniques[i])]
    if (col %in% c("factor", "character")){
      sName <- names(col)
      break
    }
  }
  return(sName)
}

makeYNames <- function(data, count) {
  data_types <- sapply(data, class)
  yNames <- vector()
  for (i in 1:length(data_types)) {
    if (data_types[i] %in% c("integer", "numeric")) {
      yNames <- c(yNames, i)
    }
    if (length(yNames) == count) break
  }
  # if no more numeric columns have been found, use the first other
  i <- 1
  while (length(yNames) < count) {
    if (!i %in% yNames) yNames <- c(yNames, i)
    i <- i + 1
  }
  return(yNames)
}