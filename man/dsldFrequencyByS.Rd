\name{dsldFrequencyByS}
\alias{dsldFrequencyByS}
\title{dsldFrequencyByS}

\description{

Assesses possible confounding relationship between a sensitive variable
S and a categorical variable C by extracting frequencies of a
combination of levels, grouped by S and C.  

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

\details{

Essentially an informal assessment of relation between S and C.
Consider the \code{svcensus} dataset.  If for instance we are studying
the effect of gender S on wage income Y, say C is occupation.  If S and
C both affect income and are related to each other, we have a confounder
issue.


}

\examples{
data(svcensus) 
dsldFrequencyByS(svcensus, cName = "educ", sName = "gender")
# not much difference in education between genders
dsldFrequencyByS(svcensus, cName = "occ", sName = "gender")
# substantial difference in occupation between genders
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

