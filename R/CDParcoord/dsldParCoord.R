#' dsldParCoord is a function that plots points in parallel coordinates
#' @param data data: The data we want to look at
#' @param yName yName: The variable we want to predict
#' @param grpName grpName: This determines what lines we are plotting
#' @return returns a plot with discparcoord

dsldParCoord <- function(data, yName, grpName) {
  discparcoord(data, k = 500, name = yName, grpcategory = grpName )
}
