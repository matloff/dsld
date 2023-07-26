#dsldFgrrm(), a wrapper for fairml::fgrrm()
#usage:
#fairml::fgrrm( response, predictors, sensitive, unfairness,
#               definition = "sp-komiyama", family = "binomial", lambda = 0,
#               save.auxiliary = FALSE)


#TODO: 
# finalize variable names
# Make wrapper accept str or double for yName
# Complete examples after completing wrapper

dsldFgrrm <- function(data, yName, xName, sName, unfairness,
                      definition = "sp-komiyama", family = "binomial", 
                      lambda = 0, save.auxiliary = FALSE)
{
  if (!require('cccp')) install.packages('cccp'); library('cccp')
  
  cc = data[complete.cases(data),]
  r = yName
  p = cc[, xName]
  s = cc[, sName]
  
  fairml::fgrrm(response = r, predictors = p, 
                sensitive = s, unfairness = unfairness,
                definition = definition, family = family, 
                lambda = lambda, save.auxiliary = save.auxiliary)
}



# Example 1.1 --- Updated Example of 1.1
# library(survival)
# data(flchain)
# d = flchain
# r = cbind(time = flchain$futime + 1, status = flchain$death)#yName --- Turn this into a valid argument in function
# p = c("sample.yr", "kappa", "lambda", "flc.grp", "creatinine", "mgus", "chapter") #xName
# s = c("age", "sex") #sName
# m = dsldFgrrm(d, r, p, s, 0.05, family = "cox")





# Example 1

# library(survival)
# data(flchain)
#
# # complete data analysis.
# flchain = flchain[complete.cases(flchain), ]
# # short-hand variable names.
# r = cbind(time = flchain$futime + 1, status = flchain$death)
# s = flchain[, c("age", "sex")]
# p = flchain[, c("sample.yr", "kappa", "lambda", "flc.grp", "creatinine", "mgus",
#                 "chapter")]
# 
# m = dsldFgrrm(r, p, s, 0.05, family = "cox")
# summary(m)
































# Example 2

# library(survival)
# data(flchain)
# # complete data analysis.
# flchain = flchain[complete.cases(flchain), ]
# # short-hand variable names.
# r = cbind(time = flchain$futime + 1, status = flchain$death)
# s = flchain[, c("age", "sex")]
# p = flchain[, c("sample.yr", "kappa", "lambda", "flc.grp", "creatinine", "mgus",
#                 "chapter")]
# ## Not run:
# m = dsldFgrrm(r, p, s, 0.05, family = "cox")
# summary(m)


# Example 3

# data(obesity.levels)
# # short-hand variable names.
# r = obesity.levels[, "NObeyesdad"]
# s = obesity.levels[, c("Gender", "Age")]
# p = obesity.levels[, setdiff(names(obesity.levels), c("NObeyesdad", "Gender", "Age"))]
# ## Not run:
# # the lambda = 0.1 is very helpful in making model estimation succeed.
# m = fgrrm(r, p, s, 0.05, family = "multinomial", lambda = 0.1)
# summary(m)

