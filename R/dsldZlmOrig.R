#dsldZlmOrig(), a wrapper for fairml::zlm.orig()
#usage:
#zlm.orig(response, predictors, sensitive, max.abs.cov)

dsldZlmOrig <- function(response, predictors, sensitive, max.abs.cov)
{
  fairml::zlm.orig(response = response, predictors = predictors,
                   sensitive = sensitive, max.abs.cov = max.abs.cov)
}

#No Examples were provided in fairml.pdf