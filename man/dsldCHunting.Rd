\name{dsldCHunting and dsldCHunting}
\alias{dsldCHunting}
\alias{dsldOHunting}
\title{Confounder and Proxy Hunting}

\description{ 
    Confounder hunting--searches for variables C that predict both Y and
    S. Proxy hunting--searches for variables O that predict S.
}

\usage{
dsldCHunting(data,yName,sName,intersectDepth=10)
dsldOHunting(data,yName,sName)
}

\arguments{
    \item{data}{
        Data frame comprising the input data.
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name of the sensitive attribute column.
    }
    \item{intersectDepth}{
        Maximum size of Y predictor set and S predictor set to intersect.
    }
}

\author{
    N. Matloff
}

\examples{  

    donttest{
    data(law.school.admissions) 
    dsldCHunting(law.school.admissions,'bar','race1')
    # e.g. suggests confounders 'cluster', 'decile3', 'decile1'
    
    data(mortgageSE)
    dsldOHunting(mortgageSE,'deny','black')
    # e.g. suggests using loan value and condo purchase as proxies
    }
}

\details{

    dsldCHunting:
    
    The specified \code{qeML} function will be on the indicated data once
    for each level of the sensitive variable.  For each such level, estimated
    regression function values will be obtained for each row in
    \code{sComparisonPts}.
    
    dsldOHunting:
    
    Factors, if any, will be converted to dummy variables, and then the
    Kendall Tau correlations will be calculated betwene S and potential O
    variales.  (The Y column will be ignored.)

}
