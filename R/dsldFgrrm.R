#dsldFgrrm(), a wrapper for fairml::fgrrm()
#usage:
#fairml::fgrrm( response, predictors, sensitive, unfairness,
#               definition = "sp-komiyama", family = "binomial", lambda = 0,
#               save.auxiliary = FALSE)

dsldFgrrm <- function(response, predictors, sensitive, unfairness,
                      definition = "sp-komiyama", family = "binomial", 
                      lambda = 0, save.auxiliary = FALSE)
{
  fairml::fgrrm(response = response, predictors = predictors, 
                sensitive = sensitive, unfairness = unfairness,
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
# m = dsldFgrrm(response = r, sensitive = s, predictors = p, family = "cox",
#           unfairness = 0.05)
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
# m = fgrrm(response = r, sensitive = s, predictors = p, family = "cox",
#           unfairness = 0.05)
# summary(m)


# Example 3

# data(obesity.levels)
# # short-hand variable names.
# r = obesity.levels[, "NObeyesdad"]
# s = obesity.levels[, c("Gender", "Age")]
# p = obesity.levels[, setdiff(names(obesity.levels), c("NObeyesdad", "Gender", "Age"))]
# ## Not run:
# # the lambda = 0.1 is very helpful in making model estimation succeed.
# m = fgrrm(response = r, sensitive = s, predictors = p, ,
#           family = "multinomial", unfairness = 0.05, lambda = 0.1)
# summary(m)

