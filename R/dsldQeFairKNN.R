dsldQeFairKNN <- function(data, yName, deweightPars, sName=NULL,
                      yesYVal=NULL, k=25, scaleX=TRUE, holdout=floor(min(1000,0.1*nrow(data)))) {
  EDFfair::qeFairKNN(data, yName, deweightPars = deweightPars, 
                     sensNames = sName, yesYVal = yesYVal, k=k,
                     scaleX = scaleX, holdout = holdout)
}

# Currently running into some issues when calling the function cannot find function getRow1(data, yName) 
# from prepData in EDF_Misc.R

# library(dsld)
# data(svcensus)
#	dsldQeFairKNN(svcensus, "wageinc", NULL, "gender")
