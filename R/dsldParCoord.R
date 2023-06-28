#' This file stores wrappers for the functions in the freqparcoord pkg

library(freqparcoord)
# library(dsld)
getSuggestedLib('freqparcoord') # Installs cdparcoord on user machine if necessary

#' dsldParCoord is a function that plots points in parallel coordinates
#' @param data The data we want to look at
#' @param m The number of lines to plot for each group
#' @param grpName What to group the data by
#' @return a plot (in parallel coordinates) with freqparcoord()
dsldParCoord <- function(data, m, columns, grpName, plot_filename) {
    freqparcoord::freqparcoord(data, m, columns, grpvar = grpName)
    ggsave(plot_filename)
}

# sample data modification using the Gender Pay dataset
# data(prgeng)
# pe <- prgeng[,c(1,3,5,7:9)]
# pe25 <- pe[pe$wageinc < 250000,]

# pe25 <- makeFactor(pe25,c('educ','occ','sex'))
# pe25disc <- discretize(pe25,nlevels=5)
# 
# dsldParCoord(pe25,10,c(1,5,6),'sex') # sample call to plot the graph
# 
# data(mlb)
# dsldParCoord(mlb,5,4:6,7) # sample call to plot the graph
# dsldParCoord(mlb,5,6,'PosCategory') # 1 col call
# data(pef)
# dsldParCoord(pef,10,1,'sex') # sample call to plot the graph
