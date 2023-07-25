#dsldFgrrm(), a wrapper for fairml::fgrrm()
#usage:
#fairml::fgrrm( response, predictors, sensitive, unfairness,
#               definition = "sp-komiyama", family = "binomial", lambda = 0,
#               save.auxiliary = FALSE)

dsldFgrrm <- function(yName, xName, sName, unfairness,
                      definition = "sp-komiyama", family = "binomial", 
                      lambda = 0, save.auxiliary = FALSE)
{
  fairml::fgrrm(response = yName, predictors = xName, 
                sensitive = sName, unfairness = unfairness,
                definition = definition, family = family, 
                lambda = lambda, save.auxiliary = save.auxiliary)
}

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

