#dsldFrrm(), a wrapper for fairml::frrm()
#usage:
#fairml::frrm(response, predictors, sensitive, unfairness,
#     definition = "sp-komiyama", lambda = 0, save.auxiliary = FALSE)

dsldFrrm <- function(response, predictors, sensitive, unfairness,
                     definition = "sp-komiyama", lambda = 0, 
                     save.auxiliary = FALSE) 
{
  fairml::frrm(response = response, predictors = predictors, 
               sensitive = sensitive, unfairness = unfairness, 
               definition = definition, lambda = lambda, 
               save.auxiliary = save.auxiliary)
}

# Example 1

# data(communities.and.crime)
# 
# # short-hand variable names.
# cc = communities.and.crime[complete.cases(communities.and.crime), ]
# r = cc[, "ViolentCrimesPerPop"]
# s = cc[, c("racepctblack", "PctForeignBorn")]
# p = cc[, setdiff(names(cc), c("ViolentCrimesPerPop", names(s)))]
# 
# m = dsldFrrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
# summary(m)

# Example 2

# data(law.school.admissions)

# # short-hand variable names.
# ll = law.school.admissions
# r = ll[, "ugpa"]
# s = ll[, c("age", "race1")]
# p = ll[, setdiff(names(ll), c("ugpa", "age", "race1"))]
# m = dsldFrrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
# summary(m)