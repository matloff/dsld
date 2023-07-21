#' This file stores wrappers for the functions in the freqparcoord pkg
#'
#' dsldFreqPCoord is a function that plots points in parallel coordinates
#' ::: Arguments :::
#' @param data The data we want to look at
#' @param m The number of lines to plot for each group
#' @param sName What to group the data by (use interaction() to combine several)
#' @param method "maxdens", "locmax" or "randsamp"
#' @param faceting "vert", "horiz" or "none"
#' @param k Number of nearest neighbors to use for density estimation
#' @param klm If method is "locmax", number of nearest neighbors to use for 
#'            finding local maxima for cluster hunting. Generally larger than k
#' @param keepidxs If not NULL, the indices of the rows of x that are plotted 
#'                 will be stored in a component idxs of the return value
#' @param plotidxs If TRUE, lines in the display will be annotated with their 
#'                 case numbers
#' @param cls Cluster, if any
#' @param plot_filename Name of the file that will hold the saved graph image
#' 
#' @return A plot (in parallel coordinates) with freqparcoord()
#' 
#' @examples
#' Example 1
#' library(dsld)
#' data(svcensus)
#' dsldFreqPCoord(svcensus,10,c(1,4,5),'gender')
#' 
#' Example 2
#' library(qeML)
#' data(mlb)
#' dsldFreqPCoord(mlb,5,4:6,7)
dsldFreqPCoord <- function(data, m, columns = 1:ncols(data), sName = NULL, 
                           method = "maxdens", faceting = "vert", k = 50, 
                           klm = 5*k, keepidxs = NULL, plotidxs = FALSE, 
                           cls = NULL, plot_filename = NULL) 
{
    # TODO: Implement plotly features

    # May need to delete these 3 library() lines
    if (!require('freqparcoord')) install.packages('freqparcoord')
    library('freqparcoord')
    if (!require('ggplot2')) install.packages('ggplot2')
    library('ggplot2')
    # library(dsld)
    
    getSuggestedLib('freqparcoord') # Installs freqparcoord on user machine if necessary
    getSuggestedLib('ggplot2')
    
    # This code allows for columns to be inputted as all strings or all ints 
    # (no mix, has to be one or the other)
    if (is.vector(columns) && all(sapply(columns, is.character))){
        # Convert strings to their corresponding column numbers
        columns <- match(columns, colnames(data))
    }
    
    # If no filename argument provided, do not save an image file, just generate the image
    if (is.null(plot_filename)){
        freqparcoord::freqparcoord(data,m,dispcols=columns,grpvar=sName,
                                   method=method,faceting=faceting,k=k,klm=klm,
                                   keepidxs=keepidxs,plotidxs=plotidxs,cls=cls)
    } else{
        freqparcoord::freqparcoord(data,m,dispcols=columns,grpvar=sName,
                                   method=method,faceting=faceting,k=k,klm=klm,
                                   keepidxs=keepidxs,plotidxs=plotidxs,cls=cls)
        ggsave(plot_filename) # Save as img
        # pr2file(plot_filename) # Doesn't work with Python, so we are leaving this commented for now
    }
}


