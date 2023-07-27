#dsldFrrm(), a wrapper for fairml::frrm()
#usage:
#fairml::frrm(response, predictors, sensitive, unfairness,
#     definition = "sp-komiyama", lambda = 0, save.auxiliary = FALSE)

dsldFrrm <- function(data, yName, sName, unfairness,
                     definition = "sp-komiyama", lambda = 0, 
                     save.auxiliary = FALSE) 
{
  cc = data[complete.cases(data), ]
  r = cc[, yName]
  p = cc[,!names(cc) %in% c(yName, sName)]
  s = cc[, sName]

  fairml::frrm(response = r, predictors = p, 
               sensitive = s, unfairness = unfairness, 
               definition = definition, lambda = lambda, 
               save.auxiliary = save.auxiliary)
}

# Example 1
# library(dsld)
# library(fairml)

# data(communities.and.crime)
# yName = "ViolentCrimesPerPop"
# sName = c("racepctblack", "PctForeignBorn")

# m = dsldFrrm(communities.and.crime, yName, sName, 0.05)
# summary(m)


# Example 2
# library(dsld)
# library(fairml)

# data(law.school.admissions)
# yName = "ugpa"
# sName = c("age", "race1")

# m = dsldFrrm(law.school.admissions, yName, sName, 0.05)
# summary(m)

