\name{dsldEDFFair Wrappers}
\alias{dsldQeFairKNN}
\alias{dsldQeFairRF}
\alias{dsldQeFairRidgeLin}
\alias{dsldQeFairRidgeLog}
\alias{predict.dsldQeFair}

\title{dsldEDFFair Wrappers}

\description{ 
  Explicitly Deweighted Features: control the effect of proxies 
  related to sensitive variables for prediction. 
}

\usage{
dsldQeFairKNN(data, yName, sNames, deweightPars=NULL, yesYVal=NULL,k=25,
  scaleX=TRUE, holdout=floor(min(1000,0.1*nrow(data))))
dsldQeFairRF(data,yName,sNames,deweightPars=NULL, nTree=500, minNodeSize=10,
  mtry = floor(sqrt(ncol(data))),yesYVal=NULL,
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
        Values for de-emphasizing variables in a split, e.g. 
        'list(age=0.2,gender=0.5)'. In the linear case,
        larger values means more deweighting, i.e. less influence of the given 
        variable on predictions. For KNN and random forests, smaller
        values mean more deweighting.
    }
    \item{scaleX}{
        Scale the features. Defaults to TRUE.
    }
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0.
    }
    \item{k}{
        Number of nearest neighbors. In functions other than 
        \code{dsldQeFairKNN} for which this is an argument, 
        it is the number of neighbors to use in finding 
        conditional probabilities via knnCalib.
    } 
    \item{holdout}{
        How many rows to use as the holdout/testing set. Can be NULL.
        The testing set is used to calculate s correlation and test accuracy.
    } 
    \item{nTree}{
        Number of trees.
    }
    \item{minNodeSize}{
        Minimum number of data points in a tree node.
    }
    \item{mtry}{
        Number of variables randomly tried at each split.
    }
    \item{object}{
        An object returned by the dsld-EDFFAIR wrapper.  
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
  
    The sensitive variables S are removed entirely, but there is concern
    that they still affect prediction indirectly, via a set C of proxy
    variables. 

    Linear EDF reduces the impact of the proxies through a shinkage
    process similar to that of ridge regression. Specifically, instead
    of minimizing the sum of squared errors SSE with respect to a
    coefficient vector b, we minimize SSE + the squared norm of Db,
    where D is a diagonal matrix with nonzero elements corresponding to
    C. Large values penalizing variables in C, thus shrinking them.

    KNN EDF reduces the weights in Euclidean distance for variables in
    C.  The random forests version reduces the probabilities that a
    proxy will be used in splitting a node.

    By using various values of the deweighting parameters, the user can
    choose a desired position in the Fairness-Utility Tradeoff.

    More details can be found in the references. 
}

\value{

The EDF functions return objects of class 'dsldQeFair', which include
components for test and base accuracy, summaries of inputs and so on.

}

\references{
  https://github.com/matloff/EDFfair 
}

\seealso{
Matloff, Norman, and Wenxi Zhang. "A novel regularization approach to fair ML." \cr
\code{arXiv preprint arXiv:2208.06557} (2022).
}

\examples{  
\donttest{
data(compas1) 
data(svcensus) 

# dsldQeFairKNN: deweight "decile score" column with "race" as 
# the sensitive variable
knnOut <- dsldQeFairKNN(compas1, "two_year_recid", "race", 
   list(decile_score=0.1), yesYVal = "Yes")
knnOut$testAcc 
knnOut$corrs 
predict(knnOut, compas1[1,-8]) 

# dsldFairRF: deweight "decile score" column with "race" as sensitive variable
rfOut <- dsldQeFairRF(compas1, "two_year_recid", "race", 
   list(decile_score=0.3), yesYVal = "Yes")
rfOut$testAcc
rfOut$corrs 
predict(rfOut, compas1[1,-8]) 

# dsldQeFairRidgeLin: deweight "occupation" and "age" columns
lin <- dsldQeFairRidgeLin(svcensus, "wageinc", "gender", deweightPars = 
  list(occ=.4, age=.2))
lin$testAcc 
lin$corrs 
predict(lin, svcensus[1,-4])

# dsldQeFairRidgeLin: deweight "decile score" column
log <- dsldQeFairRidgeLog(compas1, "two_year_recid", "race", 
  list(decile_score=0.1), yesYVal = "Yes")
log$testAcc 
log$corrs 
predict(log, compas1[1,-8])}
}     
