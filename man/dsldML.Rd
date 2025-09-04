\name{dsldML}
\alias{dsldML}
\title{dsldML}

\description{ 
Nonparametric comparison of sensitive groups.
}

\usage{
dsldML(data,yName,sName,qeMLftnName,sComparisonPts='rand5',opts=NULL)
}

\arguments{
    \item{data}{
        A data frame.
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name(s) of the sensitive attribute column(s).
    }
    \item{qeMLftnName}{
        Quoted name of a prediction function in the \code{qeML} package.
    }
    \item{sComparisonPts}{
        Data frame of one or more data points at which the regression
        function is to be estimated for each level of S.  If this is 
        'rand5', then the said data points will consist of five randomly 
        chosen rows in the original dataset.
    }
    \item{opts}{
        An R list specifying arguments for the above \code{qeML} function.
    }
}

\author{
    N. Matloff
}

\examples{  

## applying K-NN
## also works for: qeRF, qeRFranger, qeLASSO, qePolyLin/qePolyLog, qeXGBoost

data(svcensus) 

w <- dsldML(svcensus,'wageinc','gender',qeMLftnName='qeKNN',
   opts=list(k=50))
   
# prints testAcc for each level in sName and the predictions on sComparisonPts
print(w)

}

\details{

    In a linear model with no interactions, one can speak of "the"
    difference in mean Y given X across treatments, independent of X. 
    In a nonparametric analysis, there is interaction by definition,
    and one can only speak of differences across treatments for a
    specific X value. Hence the need for the argument
    \code{sComparisonPts}.

    The specified \code{qeML} function will be called on the indicated data once
    for each level of the sensitive variable.  For each such level, estimated
    regression function values will be obtained for each row in
    \code{sComparisonPts}.
}

\value{

An R list. The first component consists of the holdout-set prediction
accuracies, while the second is a data frame predicted values for each
sensitive group.

}


