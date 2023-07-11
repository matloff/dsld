\name{dsldLinear}
\alias{dsldLinear}

\title{DSLDLinear}

\description{ 
    Comparison of conditions for sensitive groups via linear models, with and without interactions.
}

\usage{
    ## Fit linear model
    dsldLinear(data, yName, sName, interactions = FALSE, new_data = NULL)

    ## summary method for class 'dsldLinear'
    summary(object)

    ## compare additional data in full-interactions case
    dsldDiffs(object, X_new)

}

\arguments{
    \item {data} {data frame used to train the linear model; will be split according to each level of sName 
    in output if interactions = TRUE [dataframe]}

    \item{yName} {a string, the response variable [character]}

    \item{sName} {a string, the sensitive variable; an R factor [character]}

    \item{interactions} {a logical value indicating whether or not to fit interactions with the sensitive variable. Defaults to FALSE [logical]}

    \item{new_data} {dataframe of new cases for which mean Y | X will be compared between each pairs of S levels. Defaults to NULL for the 
    no-interactions case [dataframe]}
}

\author{
    N. Matloff, A. Ashok, A. Mittal
}

\examples{  
#   The examples provided are done with the pef dataset; the dataset requires some columns to be converted to factors prior to running the function. 
    data(pef) # load updated dataset w/ sName as 'gender'
    pef$occ <- as.factor(pef$occ)  
    pef$gender <- as.factor(pef$gender)
    pef$educ <- as.factor(pef$educ)

#   Example 1: 'wageinc' as yName; 'gender' as sName; interactions == TRUE, new_data [dataframe]
    new_data <- data.frame(age = c(18,60), educ = c("zzzOther",'zzzOther'),wkswrkd = c(50,50), occ = c("106","106")) # we can compare wage differences for early in career vs. late in career
    lin1 = dsldLinear(pef,'wageinc','gender', interactions = TRUE, new_data)

#   Extract individiual regression coefficients AND compare differences in Y | X between each pairs of sName levels:
    summary(lin1)

#   Users can make comparisons with additional data as well using dsldDiffS
    X1  <- data.frame(age = c(18,60), educ = c("16",'16'), occ = c("141","141"),wkswrkd = c(40,40)) # Note: educLevel = '16'
    dat1 <- dsldDiffS(lin1, X1)

#   Example 2: 'wageinc' as yName; 'gender' as sName; interactions == FALSE; new_data = NULL
    lin2 = dsldLinear(pef,'wageinc','gender', interactions = FALSE)

#   Extract individiual regression coefficients AND compare differences in regression coefficients each pairs of sName levels:
    summary(lin2)
}

\details {
        The dsldLinear function fits a linear model to the response variable, yName, using all other available covariates 
        in the user provided dataset. The user may select for full interactions across the sensitive variable, sName, in which 
        case the function will fit m separate models, where m is the number of levels of sName.

        The function produces an instance of the `dsldLinear` class (an S3 object) that houses a several instances of the `dsldDiffModel` 
        class for each unique level of the sensitive column specified.

        The output of dsldLinear will store a list of useful traits pertaining the linear model; the following useful information will be stored: 
            1. yName & sName; [character] @ yName, @ sName
            2. Model; [character] @ model
            3. (Full Interactions only) New data input by user; [dataframe] @ data   
            3. Summary Output of model; [character] @ summary
            4. Coef of beta parameters; [character] @ coef
            5. Data used in the model (useful to see for interactions); [dataframe] @ data
    
        summary() is a polymorphic method that takes in an object of the 'dsldLinear' class and outputs an S3 object of the class 'dsldSummary'. 
        For the no-interactions case: the first element of dsldSummary outputs a dataframe containing information regarding individiual regression coefficients;
        the second element compares differences in regression coefficients each pairs of sName levels.

        In the full-interactions case: the first m (where m is the number of levels in sName) elements contain individual regression coefficients with 
        respect to each level in sName; the final element compares differences in Y | X between each pairs of sName levels. 
 
        In addition, the dsldDiffS() function helps users quantify possible evidence of discrimination between S levels. For the no-interactions case, 
        dsldDiffS compares differences in regression coefficients between each pairs of S levels. For the full-interactions case, dsldDiffS 
        now requires an argument, in data-frame form, of new test cases where difference in mean Y at that X value will be compared between 
        each pair of S levels.

        For no-interaction case, dsldDiffS returns a data frame with 4 columns:
        1. Pairs of S level names 
        2. Estimates of the differences
        3. Associated standard errors
        4. P-values
        There will be one row for each pair of S levels.
  
        For full-interactions case, dsldDiffs returns a data frame with 3 columns:
        1. Col. number of diffs argument
        2. Estimate of the difference in mean Y at that X value
        3. Associated std. err.
        There will be one row for each pair of S levels.
}


