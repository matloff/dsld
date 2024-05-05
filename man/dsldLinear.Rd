\name{dsldLinear}
\alias{dsldLinear}
\alias{dsldDiffSLin}
\alias{predict.dsldLM}
\alias{coef.dsldLM}
\alias{vcov.dsldLM}
\alias{summary.dsldLM}
\title{dsldLinear}

\description{ 
    Comparison of conditions for sensitive groups via linear models, with and
    without interactions.
}

\usage{
dsldLinear(data, yName, sName, interactions = FALSE, sComparisonPts = NULL, 
    useSandwich = FALSE)
dsldDiffSLin(object, sComparisonPts = NULL) 
\method{summary}{dsldLM}(object,...)
\method{predict}{dsldLM}(object,xNew,...)
\method{coef}{dsldLM}(object,...)
\method{vcov}{dsldLM}(object,...)
}

\arguments{
    \item{data}{
        Data frame used to train the linear model; will be split according to
        each level of sName in output if interactions = TRUE. 
    }
    \item{yName}{
       Name of the response variable Y column. 
    }
    \item{sName}{
        Name of the sensitive attribute S column. 
    }
    \item{interactions}{
        Logical value indicating whether or not to fit interactions with the
        sensitive variable S. 
    }
    \item{sComparisonPts}{
        Data frame of new cases for which mean Y | X will be compared across
        each pair of S levels. Must be in the same format as original data.
    }
    \item{useSandwich}{
        Whether to use the sandwich variance estimator.
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

# Example 1: 'wageinc' as yName; 'gender' as sName; interactions == TRUE 

# compare early vs late career
newData <- svcensus[c(1, 18), -c(4,6)] # age 22 vs 50
lin1 <- dsldLinear(svcensus, 'wageinc', 'gender', interactions = TRUE,
    newData)

coef(lin1)
vcov(lin1) 
summary(lin1)
predict(lin1, newData)

xNew  <- svcensus[c(10:15), -c(4,6)] # compare differences by S levels with new data
dsldDiffSLin(lin1, xNew)

# Example 2: 'wageinc' as yName; 'gender' as sName; interactions == FALSE; 
lin2 <- dsldLinear(svcensus, 'wageinc', 'gender', interactions = FALSE)
summary(lin2)
}

\note{
    The output of dsldDiffSLin is the same included in the output of summary().
}

\details{

    The \code{dsldLinear} function fits a linear model to the response
    variable Y using all other available covariates in the user
    provided dataset. The user may select for full interactions across
    the sensitive variable S. 

    The function produces an instance of the `dsldLM` class (an S3
    object) that houses m instances of class `dsldDiffModel`, one for
    each unique level of the sensitive column specified.  Instances of
    the generic functions \code{summary} and \code{coef} are provided.

    If \code{interactions is TRUE,} the function will fit m separate
    models, where m is the number of levels of S. The \code{summary} 
    will contain m+1 data frames; the first m of which will be the
    outputs from the individual models.  

    The last element will be a data frame comparing the differences
    in conditional mean Y|X for each pair of S levels, and for each
    value of X in \code{sComparisonPts}.
    The intention is to allow users to see the comparisons
    of conditions for sensitive groups via linear models, with 
    interactions.

    The \code{dsldDiffS} function allows users to compare mean Y at that
    X between each pair of S level for additional new unseen data levels
    using the model fitted from \code{dsldLinear}.

}
