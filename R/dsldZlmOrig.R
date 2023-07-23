#dsldZlmOrig(), a wrapper for fairml::zlm.orig()
#usage:
#zlm.orig(response, predictors, sensitive, max.abs.cov)

dsldZlmOrig <- function(yName, xName, sName, max.abs.cov)
{
  fairml::zlm.orig(response = yName, predictors = xName,
                   sensitive = sName, max.abs.cov = max.abs.cov)
}

#No Examples were provided in fairml.pdf