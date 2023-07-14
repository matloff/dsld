#dsldnclm(), wrapper for fairml::nclm()
#usage:
#fairml::nclm(response, predictors, sensitive, unfairness, covfun, lambda = 0, save.auxiliary = FALSE)

dsldNclm <- function (response, predictors, sensitive, unfairness, covfun, 
                     lambda = 0, save.auxiliary = FALSE) 
{
  fairml::nclm(response = response, predictors = predictors, 
               sensitive = sensitive, unfairness = unfairness, covfun = covfun,
               lambda = lambda, save.auxiliary = save.auxiliary)
}


# #Example 1

# data(communities.and.crime)

# #short-hand variable names.
# cc = communities.and.crime[complete.cases(communities.and.crime), ]
# r = cc[, "ViolentCrimesPerPop"]
# s = cc[, c("racepctblack", "PctForeignBorn")]
# p = cc[, setdiff(names(cc), c("ViolentCrimesPerPop", names(s)))]

# m = nclm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
# summary(m)
# m = frrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
# summary(m)

#Error: ‘there is no package called ‘cccp’
#solution: Install 'cccp' through CRAN