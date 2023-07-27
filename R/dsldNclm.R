#dsldnclm(), a wrapper for fairml::nclm()
#usage:
#fairml::nclm(response, predictors, sensitive, unfairness, covfun, lambda = 0, 
#             save.auxiliary = FALSE)

dsldNclm <- function (data, yName, sName, unfairness, covfun, 
                     lambda = 0, save.auxiliary = FALSE) 
{
  if (!require('cccp')) install.packages('cccp'); library('cccp')

  cc = data[complete.cases(data),]
  r = cc[, yName]
  p = cc[,!names(cc) %in% c(yName, sName)]
  s = cc[, sName]

  fairml::nclm(response = r, predictors = p, 
               sensitive = s, unfairness = unfairness, covfun = covfun,
               lambda = lambda, save.auxiliary = save.auxiliary)
  
}


# # Example 1
# library(dsld)
# library(fairml)
# library(cccp)

# # short-hand variable names.
# data(communities.and.crime)
# yName = "ViolentCrimesPerPop"
# sName = c("racepctblack", "PctForeignBorn")
# m = dsldNclm(communities.and.crime, yName, sName, 0.05)
# summary(m)

#Error: ‘there is no package called ‘cccp’
#solution: Install 'cccp' through CRAN



# #Example 2
# NOT UPDATED EXAMPLE
# library(dsld)
# library(fairml)
# library(cccp)
# data(law.school.admissions)

# # short-hand variable names.
# ll = law.school.admissions
# r = ll[, "ugpa"]
# s = ll[, c("age", "race1")]
# p = ll[, setdiff(names(ll), c("ugpa", "age", "race1"))]
# m = dsldNclm(r, p, s, 0.05)
# summary(m)
