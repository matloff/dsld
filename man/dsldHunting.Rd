\name{dsldCHunting and dsldOHunting}
\alias{dsldCHunting}
\alias{dsldOHunting}
\title{Confounder and Proxy Hunting}

\description{ 
    Confounder hunting:  searches for variables C that predict both Y and
    S. Proxy hunting:  searches for variables O that predict S.
}

\usage{
dsldCHunting(data,yName,sName,intersectDepth=10)
dsldOHunting(data,yName,sName)
}

\arguments{
    \item{data}{
        Data frame.
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name of the sensitive attribute column.
    }
    \item{intersectDepth}{
        Maximum size of intersection of the Y predictor set and 
        the S predictor se
    }
}

\details{

    \code{dsldCHunting}: The random forests function
    \code{qeML:qeRF} will be on the indicated data to indicate feature
    importance in prediction of Y (without S) and S (without Y).  Call
    these "important predictors" of Y and S.

    Then for each \code{i} from 1 to \code{intersectDepth}, the
    intersection of the top \code{i} important predictors of Y and the
    the top \code{i} important predictors of S will be reported, thus
    suggesting possible confounders. Larger values of \code{i} will
    report more potential confounders, though including progressively
    weaker ones.

    Note: Run times may be long.

    \code{dsldOHunting}: Factors, if any, will be converted to dummy
    variables, and then the Kendall Tau correlations will be calculated
    betwene S and potential proxy variables O.  (The Y column will be
    ignored.)


}

\author{
    N. Matloff
}

\examples{  
\donttest{
data(law.school.admissions) 
dsldCHunting(law.school.admissions,'bar','race1')
# e.g. suggests confounders 'decile3', 'lsat'
    
data(mortgageSE)
dsldOHunting(mortgageSE,'deny','black')
# e.g. suggests using loan value and condo purchase as proxies
}
}
