\name{dsldEDFFair Wrappers}
\alias{dsldQeFairKNN}
\alias{dsldQeFairRF}
\alias{dsldQeFairRidgeLin}
\alias{dsldQeFairRidgeLog}
\alias{predict.dsldQeFair}

\title{dsldEDFFair Wrappers}

\description{ 
  Wrappers for Explicitly Deweighted Features method for reducing bias
  toward a sensitive group in machine learning.
}

\usage{
dsldQeFairKNN(data, yName, sNames, deweightPars=NULL, yesYVal=NULL,k=25,
  scaleX=TRUE, holdout=floor(min(1000,0.1*nrow(data))))
dsldQeFairRF(data,yName,sNames,deweightPars=NULL, nTree=500, 
  minNodeSize=10, mtry = floor(sqrt(ncol(data))),yesYVal=NULL,
  holdout=floor(min(1000,0.1*nrow(data))))
dsldQeFairRidgeLin(data, yName, sNames, deweightPars = NULL, 
  holdout=floor(min(1000,0.1*nrow(data))))
dsldQeFairRidgeLog(data, yName, sNames, deweightPars = NULL, holdout =
  floor(min(1000, 0.1 * nrow(data))), yesYVal = levels(data[, yName])[2])
\method{predict}{dsldQeFair}(object,newx,...)
}

\arguments{
    \item{data}{
        Dataframe, training set.
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sNames}{
        Name(s) of the sensitive attribute column(s). 
    }
    \item{deweightPars}{
        Values for de-emphasizing variables, e.g. 
        'list(age=0.2,gender=0.5)'. Smaller values means more deweighting.
    }
    \item{scaleX}{
        Scale the features. 
    }
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0.
    }
    \item{k}{
        Number of nearest neighbors. In functions other than \code{qeKNN} 
        for which this is an argument, it is the number of 
        neighbors to use in finding conditional probabilities via
        \code{knnCalib}.
    } 
    \item{holdout}{
        Number of rows to use as the holdout/testing set. 
        The testing set is used to calculate S correlation and test accuracy.
    } 
    \item{nTree}{
        Number of trees.
    }
    \item{minNodeSize}{
        Minimum number of data points in a tree node.
    }
    \item{mtry}{
        Number of variables randomly tried at each tree split.
    }
    \item{object}{
        An object returned by the \code{dsld-EDFFAIR} wrapper.  
    }
    \item{newx}{
        New data to be predicted. Must be in the same format as original data.
    }
    \item{...}{
    Further arguments.
    }
}

\author{
    N. Matloff, A. Mittal, J. Tran
}

\details{
  
    EDF reduces the impact of each proxy feature for a sensitive
    variable, with a different amount of deweighting appliable to each
    such feature. The goal is to give the user control over the
    Fairness/Utility Tradeoff, choosing the desired weighting of
    fairness and prediction accuracy.

    More details can be found in the references. 
}

\references{
  https://github.com/matloff/EDFfair 


  Matloff, Norman, and Wenxi Zhang. "A novel regularization approach to
  fair ML." \ \code{arXiv preprint arXiv:2208.06557} (2022).

}

\examples{  
\donttest{
data(compas1) 
data(svcensus) 

# dsldQeFairKNN: deweight "decile score" column with "race" as the sensitive variable
knnOut <- dsldQeFairKNN(compas1, "two_year_recid", "race", 
   list(decile_score=.1), yesYVal = "Yes")
knnOut$testAcc 
knnOut$corrs 
predict(knnOut, compas1[1,]) 

# dsldFairRF: deweight "decile score" column with "race" as sensitive variable
rfOut <- dsldQeFairRF(compas1, "two_year_recid", "race", list(decile_score=.3), 
  yesYVal = "Yes")
rfOut$testAcc
rfOut$corrs 
predict(rfOut, fairml::compas[1,]) 

# dsldQeFairRidgeLin: deweight "occupation" and "age" columns
lin <- dsldQeFairRidgeLin(svcensus, "wageinc", "gender", deweightPars = 
  list(occ=.4, age=.2))
lin$testAcc 
lin$corrs 
predict(lin, svcensus[1,])

# dsldQeFairRidgeLin: deweight "decile score" column
log <- dsldQeFairRidgeLog(compas1, "two_year_recid", "race", 
  list(decile_score=0.1), yesYVal = "Yes")
log$testAcc 
log$corrs 
predict(log, compas1[1,])}
}     
