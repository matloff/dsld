\name{dsldLogit}
\alias{dsldLogit}
\alias{predict.dsldGLM}
\alias{coef.dsldGLM}
\alias{vcov.dsldGLM}
\alias{summary.dsldGLM}
\title{dsldLogit}

\description{ 
    Comparison of conditions for sensitive groups via logistic regression
    models, with or without interactions with the sensitive variable.
}

\usage{
dsldLogit(data, yName, sName, sComparisonPts = NULL, interactions = FALSE, 
   yesYVal)
\method{summary}{dsldGLM}(object,...)
\method{predict}{dsldGLM}(object,xNew,...)
\method{coef}{dsldGLM}(object,...)
\method{vcov}{dsldGLM}(object,...)
}

\arguments{
    \item{data}{
        Data frame used to train the logistic model; will be split according to
        each level of \code{sName} in output if \code{interactions} is TRUE. 
    }
    \item{yName}{
        Exact name of the binary response column, including any spaces or
        punctuation.
    }
    \item{sName}{
        Name of the sensitive attribute column. 
    }
    \item{interactions}{
        If TRUE, fit interactions with the sensitive variable. 
    }
    \item{sComparisonPts}{
        If \code{interactions} is TRUE, a
        data frame of new cases (minus Y,S) for which P(Y = yesYVal | X)
        will be compared between each pair of S levels. Must be
        in the same format as the original data. 
    }
    \item{yesYVal}{
        Y value to be considered 'yes', to be coded 1 rather than 0.
    }
    \item{object}{
        An object returned by \code{dsldLogit}. 
    }
    \item{xNew}{
        Dataframe to predict new cases. Must be in the same format 
        as \code{data}.
    }
    \item{...}{Further arguments.}
}

\author{
    N. Matloff, A. Mittal, A. Ashok
}

\examples{

data(lsa)

### interactions case - exclude S and Y in newData
newData <- lsa[c(2,22,222,2222),-c(8,11)]
log1 <- dsldLogit(lsa,'bar','race1', newData, interactions = TRUE, 'TRUE')

# extract results
coef(log1)
vcov(log1) 
summary(log1)

# predict new data --- one prediction for each level of S per row
predict(log1, newData)

# no interaction case - exclude Y in newData
newData <- lsa[c(2,22,222,2222),-c(11)]
log2 <- dsldLogit(data = lsa, yName = 'bar',sName = 'gender', 
                  interactions = FALSE, yesYVal = 'TRUE')

summary(log2)

# predict on newData  --- one prediction per row
predict(log2, newData)

}

\details{

    The \code{dsldLogit} function fits a logistic 
    regression model to the response variable. Interactions are handled
    as in \code{dsldLinear}. Without interactions, sensitive-group comparisons
    are differences in log odds. With separate group fits, comparisons are
    differences in predicted probabilities at \code{sComparisonPts}; their
    standard errors combine the two prediction variances assuming independent
    groups. Predictions always refer to the event selected by \code{yesYVal}.
    Explicit factor contrasts are retained when all sensitive levels are
    observed. Unused sensitive levels are dropped before fitting, which resets
    their contrasts as in \code{droplevels}.

}


\value{

An S3 object of class \code{dsldGLM}. Without interactions, the object contains
one component named for S. With interactions, it contains one component per
observed level of S. Each component stores its fitted \code{glm} model,
coefficients, training data, and factor encoding.

\code{coef} and \code{vcov} return lists of coefficient vectors and covariance
matrices. \code{summary} returns coefficient tables and sensitive-group
comparisons. \code{predict} returns a data frame with \code{row},
\code{prediction} (a probability), and \code{standardError}; separate group
fits also include \code{level} and return one prediction per group for each new
row.

}
