dsldDensityByS <- function(data, cName, sName, graphType = "plotly", fill = FALSE) {
  if (!class(data[, sName]) %in% c("factor", "character"))
    stop(paste("sName should be of factor or character data type. Consider setting this as a cName instead"))
  
  if (tolower(graphType) == "plot") 
    plotDensity(data, cName, sName, fill)
  else if (tolower(graphType) == "plotly") 
    plotlyDensity(data, cName, sName)
}

# ---- test ----
# library(dsld)
# data(svcensus)
# dsld::dsldDensityByS(svcensus, "wageinc", "educ")

# non interactable version of density graph
plotDensity <- function(data, cName, sName, fill) {
  getSuggestedLib('ggplot2')
  
  # the string of the columns to use for labels
  cNameStr <- names(data[cName])
  sNameStr <- names(data[sName])
  
  sGroups <- levels(unique(data[, sName]))
  for (i in 1:length(sGroups)) {
    den <- density(data[data[, sName] == sGroups[i], ][, cName])
    
    if (i == 1)
      plot(den, col = i, xlab = cNameStr, main = paste("Density of", cNameStr, "by", sNameStr))
    else
      lines(den, col = i)
    
    if (fill) polygon(den, col = i)
  }
  
  legend("topright", title = sNameStr, legend = sGroups, col = 1:length(sGroups), lty = 1)
}

# interactable plotly version
plotlyDensity <- function(data, cName, sName) {
  getSuggestedLib('plotly')
  
  # the strategy for allowing a slider to control for density
  # is plot one graph for each possible bandwidth on the slider.
  # the slider will select one graph to be visible at a time
  
  numGroups <- length(levels(unique(data[, sName])))
  # the string of the columns to use for labels
  cNameStr <- names(data[cName])
  sNameStr <- names(data[sName])
  
  bw <- seq(.25, 4, .25) # a vector of all the bandwidths we're using
  
  # aval <- a list of the arguments of all the lines we're going to graph
  aval <- list()
  for (i in 1:length(bw)) {
    # from plotly: creating a single group-separated density dataframe object to graph
    dens <- with(data, 
                 tapply(data[, cName], INDEX = data[, sName], density, adjust = bw[i]))
    df <- data.frame(
      x = unlist(lapply(dens, "[[", "x")),
      y = unlist(lapply(dens, "[[", "y")),
      group = rep(names(dens), each = length(dens[[1]]$x))
    )
    # all graphs are invisible by default
    aval[[i]] <- list(visible = FALSE, x = df$x, y = df$y)
  }
  # the default (notch 4 on the slider) is visible
  aval[[4]]$visible = TRUE
  
  # initial plot
  fig <- plotly::plot_ly(type = 'scatter', mode = 'lines', color = df$group)
  
  # each step changes the visible argument of each graph on the plot.
  steps <- list() 
  # for every bandwith on the slider, add the different density graphs to the plot. 
  for (i in 1:length(bw)) {
    fig <- plotly::add_lines(fig, x = aval[[i]]$x, y = aval[[i]]$y,
                             visible = aval[[i]]$visible)
    # if there are 3 groups in sName, and there are 8 bandwidths, there
    # are 24 graphs. 
    # we need to initally set all graphss visibility to false
    step <- list(
      args = list('visible', rep(FALSE, length(aval) * numGroups)),
      method = 'restyle', label = bw[i]
    )
    # and then the corresponding 3 graphs (1 for each level of sName 
    # with the same bandwidth ) to true
    step$args[[2]][1:numGroups + numGroups * i] <- TRUE
    steps[[i]] <- step
  }
  # buttons to select fill or no fill, by changing the fill argument 
  # of the plot we're graphing
  buttons <- list(
    list(
      method = "restyle",
      args = list("fill", "none"),
      label = "no fill"
    ),
    list(
      method = "restyle",
      args = list("fill", "tozeroy"),
      label = "fill"
    )
  )
  # updatemenus is the button for fill/no fill
  # sliders is the density slider
  fig <- plotly::layout(fig,
                        updatemenus = list(list(
                          active = 0,
                          x = 0,
                          y = 1,
                          buttons = buttons
                        )),
                        sliders = list(list(
                          active = 3,
                          currentvalue = list(prefix = "Adjust: "),
                          steps = steps
                        )),
                        title = paste("Density of", cNameStr, "by", sNameStr),
                        xaxis = list(title = cNameStr),
                        yaxis = list(title = "Density"),
                        legend = list(title = list(text = sNameStr))
  )
  fig
}