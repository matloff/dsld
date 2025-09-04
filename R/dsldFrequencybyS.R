dsldFrequencyByS <- function(data, cName, sName) {
  # type validation #
  # we're essentially just checking the value-type for the key columns
  if (!class(data[, sName]) %in% c("factor", "character")) {
    stop(paste(
      "sName should be of factor or character data type."
    ))
  }
  # helpful error message if the specified confounder column isn't factor
  if (!class(data[, cName]) %in% c("factor", "character")) {
    stop(paste(
      "cName should be of factor or character data type. Consider",
      " calling `dsldDensityByS(data, cName = ",
      cName,
      ", sName = ",
      sName,
      ")` instead",
      sep = ""
    ))
  }
  
  # sensitive variable frequencies #
  # unique levels to ensure order
  yGroups <- unique(data[[cName]])
  
  # get a lookup for every s level against every ylevel
  freqLookup <- table(data[[sName]], data[[cName]])
  
  # convert counts to proportions
  freqLookup <- freqLookup / rowSums(freqLookup)
  
  # convert to dataframe
  frequencies <- as.data.frame.matrix(freqLookup)
  names(frequencies) <- c(
    paste0("Frequency of ", yGroups)
  )
  
  return(frequencies)
}

