#dsldQeFairRidgeLin(), a wrapper for qeFairRidgeLin()
#usage:
#qeFairRidgeLin(data,yName,deweightPars,sensNames=NULL,
#               holdout=floor(min(1000,0.1*nrow(data))))

dsldQeFairRidgeLin <- function(data, yName, deweightPars, sName=NULL,
                               holdout=floor(min(1000,0.1*nrow(data))))
{
  EDFfair::qeFairRidgeLin(data=data, yName=yName, deweightPars=deweightPars,
                          sensNames = sName, holdout=holdout)
}