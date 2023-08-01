#dsldFgrrm(), a wrapper for fairml::fgrrm()
#usage:
#fairml::fgrrm( response, predictors, sensitive, unfairness,
#               definition = "sp-komiyama", family = "binomial", lambda = 0,
#               save.auxiliary = FALSE)


#TODO: 
# Instead of the users having to enter yName and xName try to do that
#   inside the function

dsldFgrrm <- function(data, yName, xName, sName, unfairness,
                      definition = "sp-komiyama", family = "binomial", 
                      lambda = 0, save.auxiliary = FALSE)
{
  if (!require('cccp')) install.packages('cccp'); library('cccp')
  
  cc = data[complete.cases(data),]
  r = yName

  p = xName

  s = cc[, sName]
  
  fairml::fgrrm(response = r, predictors = p, 
                sensitive = s, unfairness = unfairness,
                definition = definition, family = family, 
                lambda = lambda, save.auxiliary = save.auxiliary)
}

# Example 1
# library(survival)
# data(flchain)

# # complete data analysis.
# flchain = flchain[complete.cases(flchain), ]

# yName = cbind(time = flchain$futime + 1, status = flchain$death)
# xName = flchain[, c("sample.yr", "kappa", "lambda", "flc.grp", "creatinine", "mgus",
#                 "chapter")]
# sName = c("age", "sex")

# m = dsldFgrrm(data = flchain, yName = yName, xName = xName, sName = sName, 0.05, family = "cox")
# summary(m)


# Example 2
# library(survival)
# data(flchain)
# # complete data analysis.
# flchain = flchain[complete.cases(flchain), ]

# yName = cbind(time = flchain$futime + 1, status = flchain$death)
# sName = c("age", "sex")
# xName = flchain[, c("sample.yr", "kappa", "lambda", "flc.grp", "creatinine", "mgus",
#                 "chapter")]
# m = dsldFgrrm(data = flchain, yName = yName, xName = xName, sName = sName, 0.05, family = "cox")
# summary(m)


# Example 3
# library(fairml)
# data(obesity.levels)

# yName = obesity.levels[, "NObeyesdad"]
# sName = c("Gender", "Age")
# xName = obesity.levels[, setdiff(names(obesity.levels), c("NObeyesdad", "Gender", "Age"))]
# m = dsldFgrrm(data = obesity.levels, yName = yName, xName = xName, sName = sName, 0.05, family = "multinomial", lambda = 0.1)
# summary(m)
