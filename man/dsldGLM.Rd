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
    \item{data}{dataset in dataframe form}
    \item{yName}{as in qeML functions, the response variable}
    \item{sName}{name of the sensitive variable, an R factor}
    \item{family_type}{Type of model user wants to fit}
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

#   The user can also call summary(log_model_1), coef(log_model_1), dsldGetData(log_model_1) to extract individual components of the dsld models. 
  
#   Example 2: Using compas dataset. We fit a logistic regression model, with 'two_year_recid' as the response variable and 'race' as sensitive variable.
    data(compas)
    compass_1 <- dsldGLM(data = compas, yName = 'two_year_recid', sName = 'race', family_type = 'binomial', interactions = TRUE) # we account for interactions
    compass_2 <- dsldGLM(data = compas, yName = 'two_year_recid', sName = 'race', family_type = 'binomial', interactions = FALSE) # no interactions interactions

#   The user can also call summary(compass_1), coef(compass_1), dsldGetData(compass_1) to extract individual components of the dsld models. 

}

\note{
# A Warning About Simplification and Hypothesis Testing #
It's quite clear that statisticians and the people whom we present to, often 
like simple, well-delineated results. However, reality is often more complex 
than we'd like it to be, and any statistics we use in an attempt to model 
reality or measure it should reflect this complexity.

With this in mind, the authors of this package would like to extend an official 
warning regarding the use of Hypothesis Testing, but more generally the over-
simplification that often occurs in statistics. In this function we deal with 
linear models that attempt to force a linear pattern onto trends that may have 
a more nuanced shape. Thus, it is important to understand that any information 
regarding inequalities like gender wage gap, employment chances, etc. are 
gauging broad trends present in the data, but neglect the more nuanced 
interactions that may be present between those sensitive variables and others 
present. In other words, the wage gap may vary across occupations or across 
ages, don't let a desire for simplicity neglect this underlying trend in the 
data.

Moreover, although we provide the tools to perform it in the form of standard 
errors, hypothesis testing is similarly deemed an oversimplification of the 
reality of results. First, consider that most null hypotheses are false based 
on a priori grounds, so the results will almost always favor the expected 
results the statistician wants. Next, consider the rising opposition against 
the extreme focus on signficance testing that has recently come about, forcing 
even the American Statistical Association to release a position paper about 
the widespread mis-use of the test. Then, consider how signficance itself can 
be faked simply by virtue of a large enough dataset (as sample size grows, the 
standard error in estimation falls to zero and difference in population to 
sample starts shrinking to zero). Furthermore, consider the erroeneous 
signs of signficance as described in p-hacking, whereby random samples may 
occasionally by chance result in statistically signficant results, creating 
grounds for rejecting the null hypothesis by virtue of chance. Finally, 
consider that hypothesis testing lacks the nuanced information that a 
confidence interval provides.

Confidence intervals give a range that allows statisticians to interpret the 
margin of error (through the interval's width) and accuracy (through the 
interval's center), as well as more easily convey results to the public since 
intervals and margin of error are easier concepts to understand than p-values.

Essentially, though the package doesn't restrict users from signficance 
testing, it does promote the use of confidence intervals in replacement of S.T.
}

\seealso{
    \link{https://github.com/matloff/qeML/blob/master/inst/mdFiles/No_P_Values.md}
    \link{https://academic.oup.com/ee/article-abstract/20/5/1246/2480617?redirectedFrom=fulltext&login=false}
}


