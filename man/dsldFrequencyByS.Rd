\name{dsldFrequencyByS}
\alias{dsldFrequencyByS}
\title{dsldFrequencyByS}

\description{
    This function is designed to assess possible confounding relationship
    between a sensitive variable and a categorical variable by extracting
    frequencies of a combination of levels, grouped by a sensitive variable and
    the response variable.
}

\usage{
dsldFrequencyByS(data, cName, sName)
}

\arguments{
    \item{data}{
        Dataset with labeled column names and at least 2 columns
    }
    \item{cName}{
        Possible confounding variable column, an R factor
    }
    \item{sName}{
        Name of the sensitive variable column, an R factor
    }
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

\examples{
data(svcensus) 
dsldFrequencyByS(svcensus, cName = "educ", sName = "gender")
}

