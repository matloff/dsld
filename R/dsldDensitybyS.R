dsldDensityByS <- function(data, cName, sName, graphType = "plotly", fill = FALSE) {
  .dsld_check_data(data, c(cName, sName))
  if (!(is.factor(data[[sName]]) || is.character(data[[sName]]))) {
    stop("sName must refer to a factor or character column.")
  }
  if (!is.numeric(data[[cName]])) stop("cName must refer to a numeric column.")
  graphType <- match.arg(tolower(graphType), c("plotly", "plot"))
  data <- data[!is.na(data[[sName]]) & is.finite(data[[cName]]), , drop = FALSE]
  data[[sName]] <- droplevels(factor(data[[sName]]))
  if (!nrow(data) || any(table(data[[sName]]) < 2L)) {
    stop("Each displayed group needs at least two finite observations for a density estimate.")
  }
  if (graphType == "plot") plotDensity(data, cName, sName, fill)
  else plotlyDensity(data, cName, sName)
}

plotDensity <- function(data, cName, sName, fill) {
  group_values <- split(data[[cName]], data[[sName]])
  densities <- lapply(group_values, density)
  x_range <- range(unlist(lapply(densities, function(curve) curve$x)))
  y_range <- c(0, max(unlist(lapply(densities, function(curve) curve$y))))
  plot(NA, xlim = x_range, ylim = y_range, xlab = cName, ylab = "Density",
       main = paste("Density of", cName, "by", sName))
  for (group in seq_along(densities)) {
    if (fill) polygon(densities[[group]], col = grDevices::adjustcolor(group, alpha.f = .2), border = NA)
    lines(densities[[group]], col = group)
  }
  legend("topright", title = sName, legend = names(densities),
         col = seq_along(densities), lty = 1)
  invisible(NULL)
}

plotlyDensity <- function(data, cName, sName) {
  group_values <- split(data[[cName]], data[[sName]])
  bandwidths <- seq(.25, 4, .25)
  group_count <- length(group_values)
  colors <- grDevices::rainbow(group_count)
  figure <- plotly::plot_ly()
  steps <- vector("list", length(bandwidths))
  for (bandwidth in seq_along(bandwidths)) {
    for (group in seq_along(group_values)) {
      curve <- density(group_values[[group]], adjust = bandwidths[bandwidth])
      figure <- plotly::add_trace(figure, x = curve$x, y = curve$y,
        type = "scatter", mode = "lines", name = names(group_values)[group],
        legendgroup = names(group_values)[group], line = list(color = colors[group]),
        visible = bandwidth == 4L, showlegend = TRUE)
    }
    visible <- rep(FALSE, length(bandwidths) * group_count)
    visible[(bandwidth - 1L) * group_count + seq_len(group_count)] <- TRUE
    steps[[bandwidth]] <- list(args = list("visible", visible), method = "restyle",
                               label = bandwidths[bandwidth])
  }
  buttons <- list(
    list(method = "restyle", args = list("fill", "none"), label = "no fill"),
    list(method = "restyle", args = list("fill", "tozeroy"), label = "fill"))
  plotly::layout(figure, updatemenus = list(list(active = 0, x = 0, y = 1, buttons = buttons)),
    sliders = list(list(active = 3, currentvalue = list(prefix = "Adjust: "), steps = steps)),
    title = paste("Density of", cName, "by", sName),
    xaxis = list(title = cName), yaxis = list(title = "Density"), legend = list(title = list(text = sName)))
}
