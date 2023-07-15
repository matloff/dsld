#dsldZlrmOrig(), a wrapper for fairml:Zlrm.orig()
#usage:
#zlrm.orig(response, predictors, sensitive, max.abs.cov)

dsldZlrmOrig <- function(response, predictors, sensitive, max.abs.cov)
{
  fairml::zlrm.orig(response = response, predictors = predictors, 
                    sensitive = sensitive, max.abs.cov = max.abs.cov)
}

#No Examples were provided in fairml.pdf