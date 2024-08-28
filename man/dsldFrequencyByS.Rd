\name{dsldFrequencyByS}
\alias{dsldFrequencyByS}
\title{dsldFrequencyByS}

\description{

Informal assessment of C as a possible confounder in a relationship between a
sensitive variable S and a variable Y.

}

\usage{
dsldFrequencyByS(data, cName, sName)
}

\arguments{
    \item{data}{
        Data frame or equivalent.
    }
    \item{cName}{
        Name of the "C" column, an R factor.
    }
    \item{sName}{
        Name of the sensitive variable column, an R factor
    }
}

\details{

Essentially an informal assessment of the between S and C.  

Consider the \code{svcensus} dataset.  If for instance we are studying
the effect of gender S on wage income Y, say C is occupation.  If
different genders have different occupation patterns, then C is a
potential confounder.  (Y does not explicitly appear here.)

}

\value{Data frame, one for each level of the sensitive variable S, and
one column for each level of the confounder C. Each row sums to 1.0.}

\examples{
data(svcensus) 
dsldFrequencyByS(svcensus, cName = "educ", sName = "gender")
# not much difference in education between genders
dsldFrequencyByS(svcensus, cName = "occ", sName = "gender")
# substantial difference in occupation between genders
data(lsa)
lsa$faminc <- as.factor(lsa$fam_inc)
dsldFrequencyByS(lsa,'faminc','race1')
# distribution of family income by race
}

\author{
    N. Matloff, T. Abdullah, A. Ashok, J. Tran
}

