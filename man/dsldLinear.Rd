\name{dsldLinear}
\alias{dsldLinear}
\title{dsldLinear}

\description{ 
    Comparison of conditions for sensitive groups via linear models, with and
    without interactions.
}

\usage{
## Fit linear model
dsldLinear(data, yName, sName, interactions = FALSE, newData = NULL,
    sandwich = FALSE)

## s3 polymorphic functionalities for class 'dsldLM'
summary(dsldLM)                 # summary of all attributes
coef(dsldLM)                    # coefficients of the underlying linear model
vcov(dsldLM)
dsldDiffS(dsldLM, newData)      # compare mean Y | X for additional data in
                                # the full interactions case
}

\arguments{
    \item{data}{
        data frame used to train the linear model; will be split according to
        each level of sName in output if interactions = TRUE [dataframe]
    }
    \item{yName}{
        a string, the response variable [character]
    }
    \item{sName}{
        a string, the sensitive variable (an R factor) [character]
    }
    \item{interactions}{
        a logical value indicating whether or not to fit interactions with the
        sensitive variable. Defaults to FALSE [logical]
    }
    \item{newData}{
        dataframe of new cases for which mean Y | X will be compared between
        each pairs of S levels. Defaults to NULL for the no-interactions case
        [dataframe]
    }
}

\author{
    N. Matloff, A. Ashok, A. Mittal
}

\examples{  
# The examples provided are done with the svcensus dataset
data(svcensus) # load updated dataset w/ sName as 'gender'

# Example 1: 'wageinc' as yName; 'gender' as sName; interactions == TRUE, 
#   newData [dataframe]

# compare early vs late career
newData <- data.frame(age = c(18, 60), educ = c("zzzOther", 'zzzOther'), 
    wkswrkd = c(50, 50), occ = c("106", "106"))
lin1 <- dsldLinear(svcensus, 'wageinc', 'gender', interactions = TRUE,
    newData)

# Extract individual regression coefficients AND compare differences in 
#   Y | X between each pairs of sName levels:
summary(lin1)

# Users can also make additional comparisons with new data as well using 
#   dsldDiffS to compare mean Y|X between each pair of S levels.
xNew  <- data.frame(age = c(18, 60), educ = c("16", '16'), 
    occ = c("106", "106"), wkswrkd = c(50, 50)) # Note: educLevel = '16' 
                                                # not educLevel = 'zzzOther'
dat1 <- dsldDiffS(lin1, xNew)

# Example 2: 'wageinc' as yName; 'gender' as sName; interactions == FALSE; 
#   newData = NULL
lin2 <- dsldLinear(svcensus, 'wageinc', 'gender', interactions = FALSE)

# Extract individiual regression coefficients AND compare differences in 
#   regression coefficients between each pairs of S levels:
summary(lin2)
}

\note{
    The output of dsldDiffS is the same dataframe format as summary().
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
        5. Data used in the model (useful to see for interactions); [dataframe]
           @ data

    Furthermore, summary() is a polymorphic method that takes in an object of
    the ‘dsldLM’ class and outputs an S3 object containing several dataframes.
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
        
