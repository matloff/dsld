\name{dsldLogit}
\alias{dsldLogit}
\title{dsldLogit}

\description{ 
    Comparison of conditions for sensitive groups via logistic regression
    models, with and without interactions.
}

\usage{
## Fit logistic model
dsldLogit(data, yName, sName, sComparisonPts = NULL, interactions = FALSE, yesYVal)

## s3 polymorphic functionalities for class 'dsldGLM'
summary(dsldGLM)                # summary of all attributes
coef(dsldGLM)                   # coefficients of the underlying GLM model
vcov(dsldGLM)
dsldDiffSLog(dsldGLM, newData) 	# compare P (Y = 1|X) for additional data in the 
                                # full interactions case
predict(dsldGLM, newData)
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
    \item{sComparisonPts}{
        dataframe of new cases (minus Y,S) for which P(Y = 1| X) 
        will be compared between each pairs of S levels; defaults to NULL for 
        the no-interactions case [dataframe]
    }
    \item{yesYVal}{
        Y value to be considered "yes," to be coded 1 rather than 0.[character]
    }
    \item{dsldGLM}{
        Output from dsldLinear function
        [dsldLM]
    }
    \item{newData}{
        newData intended to compare S levels, must be in the same format as original data
        [dataframe]
    }
}

\author{
    N. Matloff, A. Ashok, A. Mittal
}

\examples{
# The examples provided are done with the lsa dataset; the
# dataset requires some minimal preparation prior to running dsldLogit.

# Example 1: ‘bar’ as yName; 'gender' as sName; interactions == TRUE,
# 			 newData [dataframe]
data(law.school.admissions) # predicting probability of passing the ‘bar’
lsa <- law.school.admissions # **abbreviated name** for ease of use

# select subset of original data for sComparisonPts argument
newData <- lsa[c(2,22,222,2222),-c(8,11)]

# we are predicting lsat score
log1 <- dsldLogit(lsa,'bar','race1', newData, interactions = TRUE, 'TRUE')

# Extract individual regression coefficients AND compare differences in
# P(Y = 'TRUE'| X) between each pairs of sName levels:
summary(log1)
            
# Users can also make additional comparisons with new data as well using
# dsldDiffS to compare mean P(Y = 1|X) between each pair of S levels.
xNew <- lsa[c(100,1000,120),-c(8,11)]
dat1 <- dsldDiffSLog(log1, xNew)

# Example 2: ‘bar’ as yName; 'gender' as sName; interactions == FALSE;
# newData = NULL
log2 <- dsldLogit(data = law.school.admissions,yName = 'bar',sName = 'gender', 
    interactions = FALSE, yesYVal = 'TRUE')

# Extract individual regression coefficients AND compare differences in
# regression coefficients between each pairs of S levels:
summary(log2)
}

\note{
    The output of dsldDiffSLog is the same dataframe format as summary().
}

\details{
    The dsldLogit function fits a logistic regression model to the response
    variable, yName, using all other available covariates in the user provided
    dataset. The user may select for full interactions across the sensitive
    variable, sName, in which case the function will fit m separate models,
    where m is the number of levels of sName.

    The function produces an instance of the `dsldGLM` class (an S3 object) that
    houses several instances of the `dsldDiffModel` class for each unique level
    of the sensitive column specified.

    The output of dsldLinear will store a list of useful traits pertaining the
    linear model; the following useful information will be stored: \cr
        1. yName & sName; [character] @ yName, @ sName \cr
        2. Model; [character] @ model \cr
        3. (Full Interactions only) New data input by user; [dataframe] @ data \cr
        4. Summary Output of model; [character] @ summary \cr
        5. Coef of beta parameters; [character] @ coef \cr
        6. Data used in the model (useful to see for interactions); [dataframe]
           @ data

    Furthermore, summary() is a polymorphic method that takes in an object of
    the ‘dsldGLM’ class and outputs an S3 object containing several dataframes.
    In the full interactions case, the first m elements (where m is the number
    of levels in sName) contain information regarding regression coefficients,
    standard errors, p-values etc. for each model fitted across each level in
    sName. The last element of the object is a dataframe comparing the
    differences in P(Y = 1 | X) for each row in the new data between each pair
    of sName levels. The intention is to allow users to see the comparisons of
    conditions for sensitive groups via logistic regression models, with and
    without interactions, in a user-friendly data frame format. For the
    no-interactions case: the first element of dsldSummary outputs a dataframe
    containing information regarding individual regression coefficients,
    standard errors, p-values etc. for the fitted model; the second element
    compares differences in regression coefficients between each pairs of sName
    levels.

    For differences in regression coefficients for no-interactions case,
    summary() returns a data frame with 4 columns: \cr
        1. Pairs of S level names  \cr 
        2. Estimates of the differences \cr
        3. Associated standard errors \cr
        4. P-values \cr
    There will be one row for each pair of S levels.
    
    For differences in P(Y = 1 | X) in the full-interactions case, summary()
    returns a data frame with 3 columns: \cr
        1. Col. number of diffs argument \cr
        2. Estimate of the difference in mean Y at that X value \cr
        3. Associated std. err. \cr
    There will be one row for each pair of S levels.

    The dsldDiffS() function allows users to compare P(Y = 1) at that X between
    each pair of S levels for additional new unseen data levels using the model
    fitted from dsldLogit. The user can input additional data if they'd like to
    make new comparisons IN addition to those from output of summary(dsldGLM).
}

