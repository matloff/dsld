
dsldFreqPCoord <- function(data, m, sName = NULL,
                           method = "maxdens", faceting = "vert", k = 50,
                           klm = 5 * k, keepidxs = NULL, plotidxs = FALSE,
                           cls = NULL, plot_filename = NULL) {

    getSuggestedLib("freqparcoord") 
    getSuggestedLib("ggplot2")

    if (!is.null(sName)) {
       s <- data[[sName]]
       scol <- which(names(data) == sName)
       dms <- data[,-scol]
       dms <- factorsToDummies(dms)
       dms <- as.data.frame(dms)
       data <- cbind(dms,s)
       data <- as.data.frame(data)
       scol <- ncol(data)
       colnames(data)[scol] <- sName
       columns <- 1:(scol-1)
    } else {
       data <- factorsToDummies(data)
       columns <- 1:ncol(data)
    }

    fpcOut <- freqparcoord::freqparcoord(
        data,
        m,
        dispcols = columns,
        grpvar = sName,
        method = method,
        faceting = faceting,
        k = k,
        klm = klm,
        keepidxs = keepidxs,
        plotidxs = plotidxs,
        cls = cls
    )
    
    if (!is.null(plot_filename)) {
        ggplot2::ggsave(plot_filename, fpcOut) # Save as img
    }
    
    return(fpcOut)
}


