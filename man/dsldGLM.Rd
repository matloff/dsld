\name{dsldGLM}
\alias{dsldGLM}

\title{DSLDGLM}

\description{
    ADD DESCRIPTION
}

\usage{
  dsldGLM <- function(data, yName, sName, family_type, interactions = TRUE)
}

\arguments{
    \item{data} {dataset in dataframe form}
    \item{yName} {as in qeML functions, the response variable}
    \item{sName} {name of the sensitive variable, an R factor}
    \item{family_type} {Type of model user wants to fit}
    \item{Interactions}{ADD}
}

\author{
    N. Matloff, A. Ashok, A. Mittal
}

\examples{
#   The dsldGLM function aims to provide a seamless experience, requiring minimal effort to utilize its
#   capabilities. Once the package is successfully downloaded and installed, the function can be leveraged by simply 
#   employing similar commands shown in the examples.
    
#   The examples are conducted using several datasets. 

#   Example 1: Using law.school.admissions dataset. We fit a logistic regression model, with 'bar' as the response variable and 'gender' as sensitive variable.
    data(law.school.admissions)
    law.school.admissions$bar <- as.integer(law.school.admissions$bar)
    law.school.admissions$bar <- as.factor(law.school.admissions$bar)
    log_model_1 <- dsldGLM(data = law.school.admissions, yName = 'bar', sName = 'gender', family_type = 'binomial', interactions = TRUE) # we account for interactions
    log_model_2 <- dsldGLM(data = law.school.admissions, yName = 'bar', sName = 'gender', family_type = 'binomial', interactions = FALSE) # no interactions interactions
  
#   Example 2: Using compas dataset. We fit a logistic regression model, with 'two_year_recid' as the response variable and 'race' as sensitive variable.
    data(compas)
    compass_1 <- dsldGLM(data = compas, yName = 'two_year_recid', sName = 'race', family_type = 'binomial', interactions = TRUE) # we account for interactions
    compass_2 <- dsldGLM(data = compas, yName = 'two_year_recid', sName = 'race', family_type = 'binomial', interactions = FALSE) # no interactions interactions
}
