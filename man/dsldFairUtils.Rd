\name{dsldFairUtils}
\alias{dsldFairUtils}
\title{dsldFairUtils}

\description{ 
Exploration of the Fairness-Utility Tradeoff.  Finds predictive accuracy
and correlation between S and predicted Y.
}

\usage{
dsldFairUtils(data, yName, sName, dsldFTNName, unfairness = NULL,
  deweightPars = NULL, yesYVal = NULL, k_folds = 5,model_args = NULL)
}

\arguments{
    \item{data}{
        Data frame. 
    }
    \item{yName}{
        Name of the response variable Y column. Y must be numeric or
        binary (two-level R factor).
    }
    \item{sName}{
        Name of the sensitive attribute S column.
    }
    \item{dsldFTNName}{
        Quoted name of one of the \pkg{fairML} or EDF functions.   
    }
    \item{unfairness}{
        Vector of unfairness values. Nonnull for the \pkg{fairML} functions.
    }
    \item{deweightPars}{
        List of deweightPars grid. Nonnull for the EDF functions.
    }
    \item{yesYVal}{
        Y value to be treated as Y = 1 for binary Y.
    }
    \item{k_folds}{
        Number of folds to use in $k$-fold cross-validation. 
        The final result is reported as the average across all folds.
    }
  
    \item{model_args}{
      A named list of additional arguments passed directly to \code{dsldFtnName}. 
      For example, \code{model_args = list(k = 25)}.
    }
}

\author{
    A.Mittal, N. Matloff
}

\examples{  

\donttest{
data(svcensus)

## regression examples shown --- also works for classification 
dsldFairUtils(svcensus, 
              'wageinc',
              'gender', 
              'dsldQeFairKNN', 
              k_folds = 5, 
              model_args = list(k = 25), 
              deweightPars = list('occ' = c(0.9,0.2), 'educ' = c(0.3, 0.9)))

dsldFairUtils(svcensus, 
              'wageinc', 
              'gender', 
              'dsldFrrm', 
              k_folds = 5, 
              unfairness = c(0.9, 0.6, 0.1,0.05, 0.005))
}
}

\details{

Tool for exploring tradeoff between utility (predictive accuracy, Mean
Absolute Prediction Error or overall probability of misclassification)
and fairness. Roughly speaking, the latter is defined as the strength of
relation between S and predicted Y (the smaller, the better).

}

\value{

A data-frame showing accuracy and correlation between predicted Y and S. 

}

