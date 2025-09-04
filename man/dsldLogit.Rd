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
        Data frame used to train the linear model; will be split according to
        each level of \code{sName} in output if \code{interactions} is TRUE. 
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name of the sensitive attribute column. 
    }
    \item{interactions}{
        If TRUE, fit interactions with the sensitive variable. 
    }
    \item{sComparisonPts}{
        If \code{interactions} is TRUE, a
        a data frame of new cases (minus Y,S) for which P(Y = 1| X) 
        will be compared between each pairs of S levels. Must be
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
    as in \code{dsldLinear}.

}


\value{

The \code{dsldLog} function returns an S3 object of class 'dsldGLM',
with one component for each level of S. Each component includes
information about the fitted model.

}
