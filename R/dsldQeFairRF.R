dsldQeFairRF <- function(data,yName,deweightPars,sName=NULL,
                     nTree=500,minNodeSize=10,mtry = floor(sqrt(ncol(data))),
                     yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data)))) {
  EDFfair::qeFairRF(data, yName, deweightPars, sensNames = sName, nTree = nTree,
                    minNodeSize = minNodeSize, mtry = mtry, yesYVal = yesYVal, 
                    holdout = holdout)
}

# library(dsld)
# data(svcensus)
# dsldQeFairRF(svcensus, "wageinc", svcensus["age", "edu", "occ"],"gender")
# Currently not working. Not quite sure what argument deweightPars is used for and what is its data type?
