#dsldZlrmOrig(), a wrapper for fairml:Zlrm.orig()
#usage:
#zlrm.orig(response, predictors, sensitive, max.abs.cov)

dsldZlrmOrig <- function(yName, xName, sName, max.abs.cov)
{
  fairml::zlrm.orig(response = yName, predictors = xName, 
                    sensitive = sName, max.abs.cov = max.abs.cov)
}

#No Examples were provided in fairml.pdf