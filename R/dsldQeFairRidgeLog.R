#dsldQeFairRidgeLog(), a wrapper for EDFfair::qeFairRidgeLog()
#usage:
#EDFfair::qeFairRidgeLog(data, yName, deweightPars, sensNames = sName, yesYVal, holdout)

dsldQeFairRidgeLog<- function(data, yName, deweightPars, sName = NULL, yesYVal = 0,
    holdout = floor(min(1000,0.1 * nrow(data)))){
  EDFfair::qeFairRidgeLog(data, yName, deweightPars, sName, yesYVal, holdout)
}

#Example: 1
#NOTE: EXAMPLE DOESN'T WORK BUT CALLS THE EDF FUNCTION
#TODO: test examples after adding functions to namespace
#library(dsld)
#data(svcensus)
#dsldQeFairRidgeLog(svcensus, 'wageinc', 1)