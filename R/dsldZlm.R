#dsldZlm(), a wrapper for fairml::zlm()
#usage:
#zlm(response, predictors, sensitive, unfairness)

dsldZlm <- function(response, predictors, sensitive, unfairness)
{
  fairml::zlm(response = response, predictors = predictors, 
              sensitive = sensitive, unfairness = unfairness)
}

#No examples were provided in fairml.pdf