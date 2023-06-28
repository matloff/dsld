\name{dsldLinearModel}
\alias{dsldLinModel}

\title{DSLD Linear Model}
\description{
    This function produces an instance of the `dsldLinModel` class that 
    houses a separate instance of the `dsldDiffModel` class for each 
    unique, interactive level in the sensitive column specified. The end 
    result is a linear model that with or without interactions that 
    predicts over the inputted data with respect to yName and sName.
}

\usage{
    dsldLinModel <- function(data, yName, sName, interactions=TRUE)
}

\arguments{
    \item{data}{dataset to model over, will be split according to each level 
    in the final outputted `dsldLinModel` object [dataframe]}
    \item{yName}{as in qeML functions, predictive variable [character]}
    \item{sName}{name of the sensitive variable, an R factor [character]}
    \item{interactions}{specifies whether or not to consider interactions. 
    Defaults to TRUE [boolean]}
}

\author{
    N. Matloff, A. Ashok, A. Mittal
}

\examples{
    # empty for now
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
errors, hypothesis testing is deemed a 
}

