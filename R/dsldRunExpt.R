#TODO: Fix Error below

#dsldRunExpt(), a wrapper for EDFfair::RunExpt()
#usage:
#EDFfair::runExpt(nReps,cmdPartial,xvals,code='')

dsldRunExpt <- function(nReps, cmdPartial, xvals, code='')
{
  EDFfair::runExpt(nReps=nReps,cmdPartial=cmdPartial,xvals=xvals,code=code)
}

#Example 1:
#dsldRunExpt(25,"qeFairRidgeLin(pef,'wageinc',list(occ=xoxo),'sex')",c(0.1,0.5))

#Error: "Error in getRow() : could not find function "getRow""