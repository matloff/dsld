\name{dsldLinear}
\alias{dsldLinear}
\alias{predict.dsldLM}
\alias{coef.dsldLM}
\alias{vcov.dsldLM}
\alias{summary.dsldLM}
\title{dsldLinear}

\description{ 
    Comparison of sensitive groups via linear models, with or
    without interactions with the sensitive variable.
}

\usage{
dsldLinear(data, yName, sName, interactions = FALSE, sComparisonPts = NULL, 
    useSandwich = FALSE)
\method{summary}{dsldLM}(object,...)
\method{predict}{dsldLM}(object,xNew,...)
\method{coef}{dsldLM}(object,...)
\method{vcov}{dsldLM}(object,...)
}

\arguments{
    \item{data}{
        Data frame. 
    }
    \item{yName}{
       Exact name of the numeric response variable Y column, including any
       spaces or punctuation.
    }
    \item{sName}{
        Name of the sensitive attribute S column. 
    }
    \item{interactions}{
        Logical value indicating whether or not to model interactions with the
        sensitive variable S. 
    }
    \item{sComparisonPts}{
        If \code{interactions} is TRUE, a data frame of new 
        cases for which mean Y | X will be compared across
        each pair of S levels. Must be in the same 
        format as original data.
    }
    \item{useSandwich}{
        If TRUE, use the "sandwich" variance estimator.
    }
    \item{object}{
        An object returned by the \code{dsldLinear} function.  
    }
    \item{xNew}{
        New data to be predicted. Must be in the same format as original data.
    }
    \item{...}{
        Further arguments.
    }
}

\author{
    N. Matloff, A. Mittal, A. Ashok
}

\examples{  
data(svcensus) 

### interactions case - exclude S and Y in newData
newData <- svcensus[c(1, 18), -c(4,6)] 
lin1 <- dsldLinear(svcensus, 'wageinc', 'gender', interactions = TRUE,
    newData)
    
# extract results
coef(lin1)
vcov(lin1) 
summary(lin1)

# predict on newData  --- one prediction for each level of S per row
predict(lin1, newData)

### no interactions case - exclude Y in newData
newData <- svcensus[c(1, 18), -c(4)] 
lin2 <- dsldLinear(svcensus, 'wageinc', 'gender', interactions = FALSE)
summary(lin2)

# predict on newData  --- one prediction per row
predict(lin2, newData)
}

\details{

    The \code{dsldLinear} function fits a linear model to the response
    variable Y using all other variables in \code{data}.  The user may
    select for interactions with the sensitive variable S. 

    The function produces an instance of the \code{dsldLM} class (an S3
    object).  Instances of the generic functions \code{summary} and
    \code{coef} are provided.

    If \code{interactions} is TRUE, the function will fit m separate
    models, where m is the number of levels of S. Then \code{summary} 
    will contain m+1 data frames; the first m of which will be the
    outputs from the individual models.  

    The m+1st data frame will compare the differences
    in conditional mean Y|X for each pair of S levels, and for each
    value of X in \code{sComparisonPts}.
    The intention is to allow users to see the comparisons
    of conditions for sensitive groups via linear models, with 
    interactions with S.

    Use \code{predict} with additional comparison rows to obtain group-specific
    predictions and their standard errors. Summary p-values use an asymptotic
    normal reference. For separate group fits, the standard error of a
    difference combines the two prediction variances assuming independent
    groups. Rank-deficient fits cannot provide the full coefficient contrasts
    or prediction standard errors and produce an error when these are requested.
    Explicit factor contrasts are retained when all sensitive levels are
    observed. Unused sensitive levels are dropped before fitting, which resets
    their contrasts as in \code{droplevels}.

}

\value{

An S3 object of class \code{dsldLM}. Without interactions, the object contains
one component named for S. With interactions, it contains one component per
observed level of S. Each component stores its fitted \code{lm} model,
coefficients, covariance matrix, training data, and factor encoding.

\code{coef} and \code{vcov} return lists of coefficient vectors and covariance
matrices. \code{summary} returns coefficient tables and sensitive-group
comparisons. \code{predict} returns a data frame with \code{row},
\code{prediction}, and \code{standardError}; separate group fits also include
\code{level} and return one prediction per group for each new row.

}
