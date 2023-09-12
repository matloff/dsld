
dsldFreqPCoord <- function(data, m, columns = 1:ncols(data), sName = NULL,
                           method = "maxdens", faceting = "vert", k = 50,
                           klm = 5 * k, keepidxs = NULL, plotidxs = FALSE,
                           cls = NULL, plot_filename = NULL) {

    # environment setup
    getSuggestedLib("freqparcoord") # Installs freqparcoord if necessary
    getSuggestedLib("ggplot2")
    
    # This code allows for columns to be inputted as all strings or all ints
    # Convert strings to their corresponding column numbers
    if (all(sapply(columns, is.character))){
        columns <- match(columns, colnames(data))
    }
    
    plot <- freqparcoord::freqparcoord(
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
    
    # If no filename argument provided, do not save an image file, just generate the image
    if (!is.null(plot_filename)) {
        ggplot2::ggsave(plot_filename, plot) # Save as img
        # pr2file(plot_filename) # Doesn't work with Python, so we are leaving this commented for now
    }

    return(plot)
}


