#' This file stores wrappers for the functions in the freqparcoord pkg


#' dsldFreqPCoord is a function that plots points in parallel coordinates
#' @param data The data we want to look at
#' @param m The number of lines to plot for each group
#' @param sName What to group the data by
#' @return a plot (in parallel coordinates) with freqparcoord()
dsldFreqPCoord <- function(data, m, columns = 1:ncols(data), sName = NULL, 
                           method = "maxdens", faceting = "vert", k = 50, 
                           klm = 5*k, keepidxs = NULL, plotidxs = FALSE, 
                           randclrs = FALSE, cls = NULL, plot_filename = NULL) 
{
    # May need to delete these 3 library() lines
    if (!require('freqparcoord')) install.packages('freqparcoord'); library('freqparcoord')
    if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
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
        freqparcoord::freqparcoord(data, m, columns, grpvar = sName, method, 
                                   faceting, k, klm, keepidxs, plotidxs, 
                                   randclrs, cls)
    } else{
        freqparcoord::freqparcoord(data, m, columns, grpvar = sName, method, 
                                   faceting, k, klm, keepidxs, plotidxs, 
                                   randclrs, cls)
        ggsave(plot_filename) # Save as img
        # pr2file(plot_filename) # Doesn't work with Python, so we are leaving this commented for now
    }
}

# sample data modification using the Gender Pay dataset
# data(prgeng)
# pe <- prgeng[,c(1,3,5,7:9)]
# pe25 <- pe[pe$wageinc < 250000,]

# pe25 <- makeFactor(pe25,c('educ','occ','sex'))
# pe25disc <- discretize(pe25,nlevels=5)
# 
# dsldFreqPCoord(pe25,10,c(1,5,6),'sex') # sample call to plot the graph
# 
# data(pef)
# dsldFreqPCoord(pef,10,c(1,5,6),'sex') # sample call to plot the graph
# dsldFreqPCoord(pef,10,c(1,5,6),'sex','pr2fileTest.png')
# data(mlb)
# dsldFreqPCoord(mlb,5,4:6,7) # sample call to plot the graph
# dsldFreqPCoord(mlb,5,6,'PosCategory') # 1 col call
