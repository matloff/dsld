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
        Data-frame used to train the linear model; will be split according to
        each level of sName in output if interactions = TRUE. 
    }
    \item{yName}{
       Name of the response variable column. 
    }
    \item{sName}{
        Name(s) of the sensitive attribute column(s). 
    }
    \item{interactions}{
        A logical value indicating whether or not to fit interactions with the
        sensitive variable. Defaults to FALSE. 
    }
    \item{sComparisonPts}{
        Dataframe of new cases for which mean Y | X will be compared across
        each pair of S levels. Must be in the same format as original data.
        Defaults to NULL for the no-interactions case.
    }
    \item{useSandwich}{
        Whether or not to use sandwich variance estimator. Defaults to FALSE.
    }
    \item{object}{
        An object returned by the dsldLinear.  
        [dsldLM]
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
    The dsldLinear function fits a linear model to the response variable, yName,
    using all other available covariates in the user provided dataset. The user
    may select for full interactions across the sensitive variable, sName, in
    which case the function will fit m separate models, where m is the number of
    levels of sName.

    The function produces an instance of the `dsldLM` class (an S3 object) that
    houses several instances of the `dsldDiffModel` class for each unique level
    of the sensitive column specified.
    
    The output of dsldLinear will store a list of useful traits pertaining the
    linear model; the following useful information will be stored:  \cr
        1. yName & sName; [character] @ yName, @ sName \cr
        2. Model; [character] @ model \cr
        3. (Full Interactions only) New data input by user; [dataframe] @ data \cr
        3. Summary Output of model; [character] @ summary \cr
        4. Coef of beta parameters; [character] @ coef \cr
        5. Covariance matrix of the model; [character] @ vcov \cr
        6. Data used in the model (useful to see for interactions); [dataframe]
           @ data

    Furthermore, summary() is a polymorphic method that takes in an object of
    the 'dsldLM' class and outputs an S3 object containing several dataframes.
    In the full interactions case, the first m elements (where m is the number
    of levels in sName) contain information regarding regression coefficients,
    standard errors, p-values etc. for each model fitted across each level in
    sName. The last element of the object is a dataframe comparing the
    differences in mean Y | X for each row in the new data between each pair of
    sName levels. The intention is to allow users to see the comparisons of
    conditions for sensitive groups via linear models, with and without
    interactions, in a user-friendly data frame format. For the no-interactions
    case: the first element of dsldSummary outputs a dataframe containing
    information regarding individual regression coefficients,  standard errors,
    p-values etc. for the fitted model; the second element compares differences
    in regression coefficients between each pairs of sName levels.

    For differences in regression coefficients for no-interactions case,
    summary() returns a data frame with 4 columns: \cr
        1. Pairs of S level names  \cr
        2. Estimates of the differences \cr
        3. Associated standard errors \cr
        4. P-values \cr
    There will be one row for each pair of S levels.

    For differences in Y | X  in the full-interactions case, summary() returns a
    data frame with 3 columns: \cr
        1. Col. number of diffs argument \cr
        2. Estimate of the difference in mean Y at that X value \cr
        3. Associated std. err. \cr
    There will be one row for each pair of S levels.

    The dsldDiffS() function allows users to compare mean Y at that X between
    each pair of S level for additional new unseen data levels using the model
    fitted from dsldLinear. The user can input additional data if they'd like to
    make new comparisons IN addition to those from output of summary(dsldLM).
}
