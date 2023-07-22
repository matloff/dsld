#dsldnclm(), a wrapper for fairml::nclm()
#usage:
#fairml::nclm(response, predictors, sensitive, unfairness, covfun, lambda = 0, 
#             save.auxiliary = FALSE)

dsldNclm <- function (yName, xName, sName, unfairness, covfun, 
                     lambda = 0, save.auxiliary = FALSE) 
{
  fairml::nclm(response = yName, predictors = xName, 
               sensitive = sName, unfairness = unfairness, covfun = covfun,
               lambda = lambda, save.auxiliary = save.auxiliary)
}


# #Example 1

# data(communities.and.crime)

# # short-hand variable names.
# cc = communities.and.crime[complete.cases(communities.and.crime), ]
# r = cc[, "ViolentCrimesPerPop"]
# s = cc[, c("racepctblack", "PctForeignBorn")]
# p = cc[, setdiff(names(cc), c("ViolentCrimesPerPop", names(s)))]

# m = dsldNclm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
# summary(m)

#Error: ‘there is no package called ‘cccp’
#solution: Install 'cccp' through CRAN



# #Example 2

# data(law.school.admissions)

# # short-hand variable names.
# ll = law.school.admissions
# r = ll[, "ugpa"]
# s = ll[, c("age", "race1")]
# p = ll[, setdiff(names(ll), c("ugpa", "age", "race1"))]
# m = dsldNclm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
# summary(m)
