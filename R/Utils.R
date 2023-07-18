
# many functions in dsld are wrappers for functions in other packages;
# in order to avoid "package bloat," we instead check for them as needed

# e.g. say a dsld function f() wraps some function in package p; then
# instead of listing p as imported etc. in the dsld DESCRIPTION file, 
# we write the top of f(), getSuggestedLib('p'); this loads p if it is
# installed on the user's machine, otherwise so informs the user

getSuggestedLib <- function(pkgName)
   if (!requireNamespace(pkgName,quietly=TRUE))
      stop(paste0(pkgName, ' not loaded'))

pr2file <- function(filename)
{
   origdev <- dev.cur()
   parts <- strsplit(filename,".",fixed=TRUE)
   nparts <- length(parts[[1]])
   suff <- parts[[1]][nparts]
   if (suff == "pdf") {
       pdf(filename)
   }
   else if (suff == "png") {
       png(filename,bg='white')
   }
   else jpeg(filename)
   devnum <- dev.cur()
   dev.set(origdev)
   dev.copy(which = devnum)
   dev.set(devnum)
   dev.off()
   dev.set(origdev)
}

#' Generates an sName of a data frame
#' @param data A data frame
#' @return A string of the name of a factor column with the least amount of unique values
#' @examples makeSName(svcensus)
makeSName <- function(data) {
  data_types <-
    sapply(data, class) # the datatypes of each column in data
  
  num_uniques <- sort(sapply(sapply(data, unique), length))
  sName <- NULL
  # how many distinct values for each column, sorted by least unique values
  for (i in 1:length(data_types)) {
    col <- data_types[names(num_uniques[i])]
    if (col %in% c("factor", "character")) {
      sName <- names(col)
      break
    }
  }
  return(sName)
}

#' Generates the yNames of a dataframe
#' @param data A data frame
#' @param count A number of how many yNames to generate
#'
#' @return A numeric vector that corresponds to one or more numeric columns in the data frame
#' @examples makeYNames(svcensus, 3)
makeYNames <- function(data, count = 1) {
  data_types <- sapply(data, class)
  yNames <- vector()
  for (i in 1:length(data_types)) {
    if (data_types[i] %in% c("integer", "numeric")) {
      yNames <- c(yNames, i)
    }
    if (length(yNames) == count)
      break
  }
  # if no more numeric columns have been found, use the first other
  i <- 1
  while (length(yNames) < count) {
    if (!i %in% yNames)
      yNames <- c(yNames, i)
    i <- i + 1
  }
  return(yNames)
}

#' Generates a list of groups that exist within a sName column of a data frame
#'
#' @param data A dataframe
#' @param sName An index or name of a preferably factor column
#' @param numGroups A number of how many sGroups to generate
#' @param sortedBy A string indicating how the sGroups are generated
#'  Possible values: 
#'    "Name" : first groups alphabetically are returned
#'    "Frequency" : most frequently occuring groups are returned
#'    "Frequency-Descending" : least frequently occuring groups are returned
#'
#' @return A vector of values that the sName of the data frame can take
#'
#' @examples makeSGroups(svcensus, "educ")
makeSGroups <-
  function(data,
           sName,
           numGroups = NULL,
           sortedBy = "Name") {
    # If there are 8 possible types the group variable can be, the vector is 8 long.
    # Sorted according to user
    sGroups <- NULL
    switch(
      sortedBy,
      "Name" = sGroups <- levels(unique(data[, sName])),
      "Frequency" = sGroups <-
        names(sort(table(data[, sName]), decreasing = T)),
      "Frequency-Descending" = sGroups <-
        names(sort(table(data[, sName]), decreasing = F))
    )
    # otherwise the vector is cut off to only have numGroups number of sGroups
    if (!is.null(numGroups) &&
        length(sGroups) > numGroups)
      sGroups <- sGroups[1:numGroups]
    return(sGroups)
  }


#' Restricts the values of a data frame to specified limits
#'
#' @param data A data frame
#' @param yNames A preferably numeric vector that contains the indices/names 
#' of the columns by which the data frame is restricted
#' @param xlim A vector containing (min, max) to limit the 1st item of yNames to.
#' @param ylim A vector containing (min, max) to limit the 2nd item of yNames to.
#' @param zlim A vector containing (min, max) to limit the 3rd item of yNames to.
#'
#' @return A data frame that may or not be smaller than the original data frame
#'
#' @examples limitRange(svcensus, "wageinc", c(0, 200000))
limitRange <-
  function(data,
           yNames,
           xlim = NULL,
           ylim = NULL,
           zlim = NULL) {
    # in case the user only gives lim as a single number
    xlim <- rep(xlim, 2)
    ylim <- rep(ylim, 2)
    zlim <- rep(zlim, 2)
    # limits the data frame 
    if (!is.null(xlim))
      data <-
      data[data[, yNames[1]] >= xlim[1] & data[, yNames[1]] <= xlim[2], ]
    if (!is.null(ylim))
      data <-
      data[data[, yNames[2]] >= ylim[1] & data[, yNames[2]] <= ylim[2], ]
    if (!is.null(zlim))
      data <-
      data[data[, yNames[3]] >= zlim[1] & data[, yNames[3]] <= zlim[2], ]
    return(data)
  }
