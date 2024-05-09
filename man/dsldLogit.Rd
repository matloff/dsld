\name{dsldLogit}
\alias{dsldLogit}
\alias{dsldDiffSLog}
\alias{predict.dsldGLM}
\alias{coef.dsldGLM}
\alias{vcov.dsldGLM}
\alias{summary.dsldGLM}
\title{dsldLogit}

\description{ 
    Comparison of conditions for sensitive groups via logistic regression
    models, with and without interactions.
}

\usage{
dsldLogit(data, yName, sName, sComparisonPts = NULL, interactions = FALSE, 
   yesYVal)
dsldDiffSLog(object, sComparisonPts = NULL) 
\method{summary}{dsldGLM}(object,...)
\method{predict}{dsldGLM}(object,xNew,...)
\method{coef}{dsldGLM}(object,...)
\method{vcov}{dsldGLM}(object,...)
}

\arguments{
    \item{data}{
        Data frame used to train the linear model; will be split according to
        each level of \code{sName} in output if \code{interactions} = TRUE. 
    }
    \item{yName}{
        Name of the response variable column. 
    }
    \item{sName}{
        Name of the sensitive attribute column. 
    }
    \item{interactions}{
        A logical value indicating whether or not to fit interactions with the
        sensitive variable. Defaults to FALSE.
    }
    \item{sComparisonPts}{
        Dataframe of new cases (minus Y,S) for which P(Y = 1| X) 
        will be compared between each pairs of S levels. Must be
        in the same format as the original data. Defaults to NULL for 
        the no-interactions case. 
    }
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0.
    }
    \item{object}{
        An object returned by the dsldLogit. 
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
data(law.school.admissions) 
lsa <- law.school.admissions 

newData <- lsa[c(2,22,222,2222),-c(8,11)]
log1 <- dsldLogit(lsa,'bar','race1', newData, interactions = TRUE, 'TRUE')

coef(log1)
vcov(log1) 
summary(log1) # compare difference among S levels
predict(log1, newData)
            
# compare differences by S levels in new data
xNew <- lsa[c(100,1000,120),-c(8,11)]
dsldDiffSLog(log1, xNew)

log2 <- dsldLogit(data = law.school.admissions,yName = 'bar',sName = 'gender', 
    interactions = FALSE, yesYVal = 'TRUE')
summary(log2)
}

\details{

    The \code{dsldLogit} function fits a logistic 
    regression model to the response variable. Interactions are handled
    as in \code{dsldLinear}.

}

