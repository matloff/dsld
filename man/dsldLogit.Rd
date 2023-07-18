\name{dsldLogit}
\alias{dsldLogit}

\title{DSLDLogit}

\description{ 
    Comparison of conditions for sensitive groups via logistic regression models, with and without interactions.
}

\usage{
    ## Fit linear model
    dsldLogit(data, yName, sName, interactions = FALSE, new_data = NULL)

    ## s3 polymorphic functionalities for class 'dsldLogit'
    summary(dsldLogit)
    dsldDiffS(dsldLogit, newData) # compare mean Y | X for additional data in
                                   # the full interactions case
}

\arguments{
    \item{data}{data frame used to train the linear model; will be split according to each level of sName in output if interactions = TRUE [dataframe]}
    \item{yName}{a string, the response variable [character]}
    \item{sName}{a string, the sensitive variable (an R factor) [character]}
    \item{interactions}{a logical value indicating whether or not to fit interactions with the sensitive variable. Defaults to FALSE [logical]}
    \item{new_data}{dataframe of new cases for which mean Y | X will be compared between each pairs of S levels. Defaults to NULL for the 
    no-interactions case [dataframe]}
    \item{dsldLogit}{output of dsldLinear, an s3 object of class 'dsldLogit' [list]}
}

\author{
    N. Matloff, A. Ashok, A. Mittal
}
