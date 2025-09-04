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
dsldQeFairKNN(data, yName, sNames, deweightPars = NULL, 
  yesYVal = NULL, k = 25, scaleX = TRUE)
dsldQeFairRF(data, yName, sNames, deweightPars = NULL, nTree = 500, 
  minNodeSize = 10, mtry = floor(sqrt(ncol(data))), yesYVal = NULL)
dsldQeFairRidgeLin(data, yName, sNames, deweightPars = NULL)
dsldQeFairRidgeLog(data, yName, sNames, deweightPars = NULL, yesYVal)
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
    
    The DSLD package extends functionality by providing both accuracy 
    (MAPE or misclassification rate) and fairness (correlation) on the 
    training set during model training.
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
# regression example
data(svcensus)

# test/train splits
n <- nrow(svcensus)
train_idx <- sample(seq_len(n), size = 0.7 * n) 
train <- svcensus[train_idx, ]
test  <- svcensus[-train_idx, -4]
test_y <- svcensus[-train_idx, 4]

# dsldQeFairRidgeLin: deweight "occupation" and "age" columns
### also works for qeFairKNN and qeFairRF
lin <- dsldQeFairRidgeLin(train, "wageinc", "gender", deweightPars = 
                            list(occ=.4, age=.2))

# training results
lin$trainAcc
lin$trainCorrs

# testing results
res <- predict(lin, test) 
res$correlations
mean(abs(res$preds - test_y))

# also works with dsldQeFairRF, dsldQeFairKNN


# classification example
data(compas1) 

# test/train splits
n <- nrow(compas1)
train_idx <- sample(seq_len(n), size = 0.7 * n) 
train <- compas1[train_idx, ]
test  <- compas1[-train_idx, -8]
test_y <- compas1[-train_idx, 8]
test_y <- as.factor(as.integer(test_y== 'Yes'))

# dsldQeFairKNN: deweight "decile score" column with "race" as the sensitive variable
# also works for qeFairRF, qeFairRidgeLog
knnOut <- dsldQeFairKNN(compas1, "two_year_recid", "race", 
                        list(decile_score=0.1), yesYVal = "Yes")

# training/testing results
knnOut$trainAcc 
knnOut$trainCorrs 
res = predict(knnOut, test) 
res$correlations
mean(test_y != round(res$preds$probs))

# also works with dsldQeFairRF, dsldQeFairRidgeLog
} 

}