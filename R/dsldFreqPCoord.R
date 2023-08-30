
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


