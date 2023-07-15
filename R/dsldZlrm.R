#dsldZlrm(), a wrapper for fairml::zlrm()
#usage:
#fairml::zlrm(response, predictors, sensitive, unfairness)

dsldZlrm <- function(response, predictors, sensitive, unfairness)
{
  fairml::zlrm(response = response, predictors = predictors,
               sensitive = sensitive, unfairness = unfairness)
}

# Example 1

# data(adult)
# # short-hand variable names.
# r = adult[, "income"]
# s = adult[, c("sex", "race")]
# p = adult[, setdiff(names(adult), c("income", "sex", "race"))]
# ## Not run:
# m = dsldZlrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
# summary(m)

#Error: ‘there is no package called ‘CVXR’’
#solution: Install 'CVXR' through CRAN
#Error: Does not halt
#After force quitting: 'Warning message: glm.fit: fitted probabilities numerically 0 or 1 occurred'



# Example 2

# data(bank)
# # remove loans with unknown status, the corresponding coefficient is NA in glm().
# bank = bank[bank$loan != "unknown", ]
# # short-hand variable names.
# r = bank[, "subscribed"]
# s = bank[, c("age")]
# p = bank[, setdiff(names(bank), c("subscribed", "age"))]
# m = dsldZlrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
# summary(m)


