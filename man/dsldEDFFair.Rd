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
        Training data frame with complete, finite values.
    }
    \item{yName}{
        Name of the response variable column.
    }
    \item{sNames}{
        Name(s) of the sensitive attribute column(s).
    }
    \item{deweightPars}{
        A named list of nonnegative weights for predictor columns, excluding
        Y and S, for example \code{list(occ = 0.2)}. Larger values impose
        stronger penalties in the ridge variants. Smaller values reduce
        influence in KNN and random forests. With \code{NULL}, ridge penalty
        weights are zero and KNN/random-forest weights are one.
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

    Linear EDF reduces the impact of the proxies through a shrinkage
    process similar to that of ridge regression. Specifically, instead
    of minimizing the sum of squared errors SSE with respect to a
    coefficient vector b, we minimize SSE + the squared norm of Db,
    where D is a diagonal matrix with nonzero elements corresponding to
    C. Larger values penalize those coefficients more strongly. The linear
    intercept is not penalized. The logistic variant retains the EDF
    pseudo-observation construction; it is not ordinary logistic ridge regression.
    Its appended negative-outcome observations remain present even when the
    penalty weights are zero, so it need not equal an unaugmented logistic fit.

    KNN EDF reduces the weights in Euclidean distance for variables in
    C.  The random forests version reduces the probabilities that a
    proxy will be used in splitting a node.

    By using various values of the deweighting parameters, the user can
    choose a desired position in the Fairness-Utility Tradeoff.

    More details can be found in the references.

    The DSLD package extends functionality by providing both accuracy
    (mean absolute error or misclassification rate) and fairness (correlation) on the
    training set during model training.
}

\value{

The EDF functions return objects of class 'dsldQeFair', which include
the fitted \code{base} model, training preprocessing information,
\code{trainPreds}, \code{trainAcc}, and \code{trainCorrs}. Despite its
historical name, \code{trainAcc} is an error: MAE for regression or
misclassification rate for binary classification (lower is better).

\code{predict()} returns \code{preds} and \code{correlations}. Binary
classification predictions include \code{predClasses} in the original labels
and \code{probs}. RF probabilities have columns 0 and 1; column 1 is the
probability of \code{yesYVal}. KNN and logistic probabilities describe that
positive class directly. Correlations consistently use the positive-class
probability. Undefined correlations are reported as \code{NA}.

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
knnOut <- dsldQeFairKNN(train, "two_year_recid", "race",
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
