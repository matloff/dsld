\name{dsldFairML Wrappers}
\alias{dsldFrrm}
\alias{dsldFgrrm}
\alias{dsldNclm}
\alias{dsldZlm}
\alias{dsldZlrm}
\alias{predict.dsldFairML}
\alias{summary.dsldFairML}

\title{dsldFairML Wrappers}

\description{
    Fair machine learning models: estimation and prediction. The following 
    functions provide wrappers for some functions in the \pkg{fairML}
    package. 
}
\usage{
dsldFrrm(data, yName, sName, unfairness, definition = "sp-komiyama", 
   lambda = 0, save.auxiliary = FALSE)
dsldFgrrm(data, yName, sName, unfairness, definition = "sp-komiyama", 
   family = "binomial", lambda = 0, save.auxiliary = FALSE, yesYVal)
dsldNclm(data, yName, sName, unfairness, covfun = cov, lambda = 0, 
   save.auxiliary = FALSE)
dsldZlm(data, yName, sName, unfairness)
dsldZlrm(data, yName, sName, unfairness, yesYVal)
}

\arguments{
    \item{data}{
        Data frame.
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name(s) of the sensitive attribute column(s). 
    }
    \item{unfairness}{
        A number in (0, 1]. Degree of unfairness allowed in
        the model.  A value (very near) 0 means the model is completely 
        fair, while a value of 1 means the model is not 
        constrained to be fair at all.
    }
    \item{covfun}{
        A function computing covariance matrices. 
    }
    \item{definition}{
        Character string, the label of the definition of fairness.
        Currently either 'sp-komiyama', 'eo-komiyama' or 'if-berk'.
    }
    \item{family}{
        A character string, either 'gaussian' to fit linear regression, 
        'binomial' for logistic regression, 'poisson' for
        log-linear regression, 'cox' for Cox proportional 
        hazards regression, or 'multinomial' for
        multinomial logistic regression.
    }
    \item{lambda}{
        Non-negative number, a ridge-regression penalty coefficient. 
    }
    \item{save.auxiliary}{
        A logical value, whether to save the fitted values and the residuals 
        of the auxiliary model that constructs the debiased predictors. 
    }
    \item{yesYVal}{
        Y value to be considered 'yes', to be coded 1 rather than 0.
    }
}

\details{

   See documentation for the \pkg{fairml} package.
   
   The DSLD package extends functionality by providing both accuracy 
   (MAPE or misclassification rate) and fairness (correlation) on the 
   training set when fitting the model.

}

\value{
    An object of class 'dsldFairML', which includes the model 
    information, \code{yName}, \code{sName}, and model training details.
}

\author{
    A. Mittal, S. Martha, B. Ouattara, B. Zarate, J. Tran
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

# train frrm model // also works with nclm and zlm
frrmOut <- dsldFrrm(data = train, yName = 'wageinc', sName = 'gender', 
                    unfairness = 0.2, definition = "sp-komiyama") 

# training results
summary(frrmOut)
frrmOut$trainCorrs
frrmOut$trainAcc

# testing results
res <- predict(frrmOut, test) 
res$correlations
mean(abs(res$preds - test_y))

# also works with dsldNclm, dsldZlm

# classification example
data(compas1)

# test/train splits
n <- nrow(compas1)
train_idx <- sample(seq_len(n), size = 0.7 * n) 
train <- compas1[train_idx, ]
test  <- compas1[-train_idx, -8]
test_y <- compas1[-train_idx, 8]
test_y <- as.factor(as.integer(test_y== 'Yes'))

# train fgrrm model // also works with zlrm
fgrrmOut <- dsldFgrrm(train, yName = "two_year_recid", 
                      sName = "age", unfairness = 0.05, 
                      definition = "sp-komiyama", 
                      yesYVal = 'Yes')  
# training results
summary(fgrrmOut)
fgrrmOut$trainCorrs
fgrrmOut$trainAcc

# testing results
res <- predict(fgrrmOut, test) 
res$correlations
mean(test_y != round(res$preds))

# also works with dsldZlm
}

}
