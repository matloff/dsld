\name{dsldCHunting}
\alias{dsldCHunting}
\title{dsldCHunting}

\description{ 
Confounder hunting. Searches for variables C that predict both Y and S.
}

\usage{
dsldCHunting(data,yName,sName,intersectDepth=10)
}

\arguments{
    \item{dataName}{Data frame comprising the input data.
    }
    \item{yName}{
    Quoted name of Y.
    }
    \item{sName}{
    Quoted name of the sensitive variable S, an R factor.
    }

    \itm{intersectDepth}{
    Maximum size of Y predictor set and S predictor set to intersect.
    }
}

\author{
N. Matloff
}

\examples{  

data(law.school.admissions) 
dsldCHunting(law.school.admissions,'lsat','race1')
# e.g. suggests confounders 'cluster', 'decile3', 'decile1'

\details{

The specified \code{qeML} function will be on the indicated data once
for each level of the sensitive variable.  For each such level, estimated
regression function values will be obtained for each row in
\code{sComparisonPts}.

}

