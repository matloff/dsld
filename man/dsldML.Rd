\name{dsldML}
\alias{dsldML}
\title{dsldML}

\description{ 
Comparison of conditions for sensitive groups via ML algorithsm.
}

\usage{
    dsldML(dataName,yName,sName,sComparisonPts="rand5",qeMLftnName,opts=NULL)
}

\arguments{
    \item{dataName}{Quoted name of the data frame comprising the input data.
    }
    \item{yName}{
    Name of Y, expressed via the \code{quote} function.
    }
    \item{sName}{
    Quoted name of the sensitive variable S, an R factor.
    }
    \item{sComparisonPts}{
    Data frame of one or more data points at which the regression
    function is to be estimated.  If this is 'rand5', then the said data
    points will consist of five randomly chosen rows in the original
    dataset.
    }
    \item{qeMLftnName}{
    Quoted name of a prediction function in the \code{qeML} package.
    }
    \item{opts}{
    An R list specifying arguments for the above \code{qeML} function.
    }
}

\author{
N. Matloff
}

\examples{  

data(svcensus) 
w<-dsldML('svcensus',quote(wageinc),'gender',
   qeMLftnName='qeKNN',opts=list(k=50))
print(w)

\details{

The specified \code{qeML} function will be called on the indicated data once
for each level of the sensitive variable.  For each such level, estimated
regression function values will be obtained for each row in
\code{sComparisonPts}.

}

