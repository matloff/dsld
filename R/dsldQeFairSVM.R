#dsldQeFairSVM(), a wrapper for EDFfair::qeFairSVM()
#usage:
#EDFfair::qeFairSVM(data, yName, expansion, sensNames = sName, holdout)

dsldQeFairSVM<- function(data, yName, expansion, sName = NULL,
   holdout = floor(min(1000,0.1*nrow(data)))) {
  EDFfair::qeFairSVM(data, yName, expansion, sName, holdout)
}

#Example: 1
#NOTE: EXAMPLE DOESN'T WORK BUT CALLS THE EDF FUNCTION
#TODO: test examples after adding functions to namespace
#library(dsld)
#data(svcensus)
#dsldQeFairSVM(svcensus, 'wageinc', 1)