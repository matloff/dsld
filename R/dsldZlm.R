#dsldZlm(), a wrapper for fairml::zlm()
#usage:
#zlm(response, predictors, sensitive, unfairness)

dsldZlm <- function(yName, xName, sName, unfairness)
{
  fairml::zlm(response = yName, predictors = xName, 
              sensitive = sName, unfairness = unfairness)
}

#No examples were provided in fairml.pdf