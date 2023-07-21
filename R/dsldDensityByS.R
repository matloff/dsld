# TODO
# change whole setup to using plotly

#' Graphs densities of a response variable, grouped by a sensitive variable
#'
#' @param data A dataframe with 1 numerical column and a factor column
#' @param yName A name or index of the numerical column
#' @param sName A name or index of the factor column
#' @param fill A logical value determining if the graphed curve should be filled in
#'
#' @export
#'
#' @examples
#' library(dsld)
#' data(svcensus)
#' dsldDensityByS(svcensus)
dsldDensityByS <-
  function(data,
           yName = NULL,
           sName = NULL,
           fill = FALSE) {
    if (is.null(sName))
      sName <- makeSName(data)
    else if (!class(data[, sName]) %in% c("factor", "character"))
      stop(
        "sName should be of factor or character data type. Consider setting this as a yName instead"
      )
    
    # for now, if theres no sName, this makes one so the function doesnt break
    if (is.null(sName)) {
      Group <- as.factor(rep(1, length(data[, 1])))
      data <- cbind(data, Group)
      sName <- length(data)
    }
    
    # yNames <- a vector of 2 ints/strings that correspond to the columns to be used for
    # the 2 axis on the graph. The user can specify the cols or
    # yNames will be the first 2 columns that are of numeric or integer data type
    if (is.null(yName))
      yName <- makeYNames(data, 1)
    
    sGroups <- levels(unique(data[, sName]))
    
    yNameStr <- names(data[yName])
    sNameStr <- names(data[sName])
    
    filltype = 'none'
    if (fill) filltype = 'tozeroy'
    
    for (i in 1:length(sGroups)) {
      den <- density(data[data[, sName] == sGroups[i],][, yName])
      
      if (i == 1)
        fig <-
          plotly::plot_ly(
            x = den$x,
            y = den$y,
            type = 'scatter',
            mode = 'lines',
            name = sGroups[i],
            fill = filltype
          )
      else
        fig <-
          plotly::add_trace(
            fig,
            x = den$x,
            y = den$y,
            mode = 'lines',
            name = sGroups[i]
          )
    }
    
    fig <- plotly::layout(
      fig,
      title = paste("Density of", yNameStr, "by", sNameStr),
      xaxis = list(title = yNameStr),
      yaxis = list(title = "Density"),
      legend = list(title = list(text = sNameStr))
    )
    
    fig
    
  }
