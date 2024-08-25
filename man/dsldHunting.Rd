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
        the S predictor set
    }
}

\details{

    \code{dsldCHunting}: The random forests function
    \code{qeML:qeRF} will be run on the indicated data to indicate feature
    importance in prediction of Y (without S) and S (without Y).  Call
    these "important predictors" of Y and S.

    Then for each \code{i} from 1 to \code{intersectDepth}, the
    intersection of the top \code{i} important predictors of Y and the
    the top \code{i} important predictors of S will be reported, thus
    suggesting possible confounders. Larger values of \code{i} will
    report more potential confounders, though including progressively
    weaker ones. 

    The analyst then may then consider omitting the variables C from
    models of the effect of S on Y.

    Note: Run times may be long.

    \code{dsldOHunting}: Factors, if any, will be converted to dummy
    variables, and then the Kendall Tau correlations will be calculated
    betwene S and potential proxy variables O, i.e. every column other
    than Y and S.  (The Y column itself doesn't enter into computation.)

    In fairness analyses, in which one desires to either eliminate or
    reduce the impact of S, one must consider the indirect effect of S
    via O. One may wish to eliminate or reduce the role of O.

}

\author{
    N. Matloff
}

\value{

The function \code{dsldCHunting} returns an R list, one component for
each confounder set found.

The function \code{dsldOHunting} returns an R matrix of correlations, 
one row for each level of S.

}

\examples{  
\donttest{
data(lsa) 
dsldCHunting(lsa,'bar','race1')
# e.g. suggests confounders 'decile3', 'lsat'
    
data(mortgageSE)
dsldOHunting(mortgageSE,'deny','black')
# e.g. suggests using loan value and condo purchase as proxies
}
}
