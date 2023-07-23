#dsldQeSU(), a wrapper for EDFfair::qeSU()
#usage:
#EDFfair::qeSU(data, yName, deweightPars, sensNames,lambda, yesYVal, holdout)

dsldQeSU <- function(data, yName, deweightPars, 
                     sName, lambda, yesYVal=NULL,
                     holdout=floor(min(1000,0.1*nrow(data))))
{
  EDFfair::qeSU(data=data,yName=yName,deweightPars=deqeightPars,
            sensNames=sName,lambda=lambda, yesYVal=yesYVal,
            holdout=holdout)
}

#No examples found