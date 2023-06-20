#' This file stores wrappers for the functions in the cdparcoord pkg

# library(cdparcoord)
# library(dsld)
getSuggestedLib('cdparcoord') # Installs cdparcoord on user machine if necessary


#' dsldParCoord is a function that plots points in parallel coordinates
#' @param data The data we want to look at
#' @param yName The variable we want to predict
#' @param grpName This determines what lines we are plotting
#' @return a plot (in parallel coordinates) with discparcoord()
dsldParCoord <- function(data, yName, grpName) {
  #' Questions: 
  #' Do we need to use the prefix cdparcoord::
  #' How do we decide the k value
  #' How should we use the graph to predict the Y variable passed in as an arg
  discparcoord(data, k = 500, name = yName, grpcategory = grpName )
}

# sample data modification as per the quickstart guide
data(prgeng)
pe <- prgeng[,c(1,3,5,7:9)]
pe25 <- pe[pe$wageinc < 250000,]
pe25 <- makeFactor(pe25,c('educ','occ','sex'))
pe25disc <- discretize(pe25,nlevels=5)  

dsldParCoord(pe25disc, 'pe25 Sex DiscGraph', 'sex') # sample call to plot the graph