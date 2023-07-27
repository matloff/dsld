\name{dsldPyLinear}
\alias{dsldPyLinear}

\title{dsldPyLinear}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R 
    function dsldLinear() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the 
    arguments to the R function. These Python arguments are converted to R 
    arguments that are then passed into the R function. This way, the Python 
    user can call the function similar to how an R user would call the function. 

    For more information on the R function itself, please refer to its man page, 
    which you can access through the command ?dsldLinear
}

\usage{
    ## Fit linear model
    dsldPyLinear(data, yName, sName, interactions = FALSE, new_data = R_NULL, 
                       returnType = "R")

    ## s3 polymorphic functionalities for class 'dsldLinear'
    dsldPyLinearSummary(dsldLinear)
    # compare mean Y | X for additional data in the full interactions case
    dsldPyDiffS(dsldLinear, newData = R_NULL, returnType = "R") 
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
    \item{new_data}{
        dataframe of new cases for which mean Y | X will be compared between 
        each pairs of S levels. Defaults to NULL for the no-interactions 
        case [dataframe]
    }
    \item{dsldLinear}{
        output of dsldLinear, an s3 object of class 'dsldLinear' [list]
        
        This must be an R object, not a Python object
    }
    \item{returnType}{
        A string argument that determines whether the user receieves an R result 
        (indicated by the string "R" or "r") or a Python result 
        (indicated by the string "Py", "py", "Python" or "python").
        
        For dsldPyLinear, this result is an R object or a Python object 
        (an instance of the Python DsldLinear class).
        For dsldPyDiffS, this result is an R dataframe or a Pandas dataframe.
    }
}

\note{
    The usage of this function requires Python 3.10. It might not work on linux
    machines.
}    

\note{ 
    If the user intends to pass in an R dataframe by using the R function 
    write.csv to convert from rdata to csv may create a csv with an extra 
    column. This extra column has an empty name(“”). This change to the data has 
    been known to significantly change the results when fetching with pandas. In 
    order to fix this issue, the entire column with the empty name or “” should 
    be deleted.
}

\note{
    It is better to run through the Python Shell Prompt. If you run through the 
    OS Shell Prompt instead, you have to enter the arguments in their proper 
    order -- in the Python Shell Prompt, this is not required, as you can 
    instead use the convention argName=argValue. In OS Shell Prompt, you cannot 
    specify the argName, only the argValue, so the order must be correct.
}

\author{
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{    
    # IMPORTANT: Before running any of these examples, go to /dsld/inst/Python

    # IMPORTANT: It is better to run through the Python Shell Prompt (see 
    # examples 1b and 2b). If you run through the OS Shell Prompt instead, you 
    # have to enter the arguments in their proper order. See the above note for 
    # more information.

    import rpy2.robjects as robjects
    from dsldLinear_Py_R import dsldPyLinear, dsldPyDiffS, dsldPyLinearSummary

    robjects.r['data']('svcensus')
    robjects.r('svcensus$occ <- as.factor(svcensus$occ)')
    robjects.r('svcensus$gender <- as.factor(svcensus$gender)')
    robjects.r('svcensus$educ <- as.factor(svcensus$educ)')

    robjects.r('new_data <- data.frame(age = c(18, 60), educ = c("zzzOther", "zzzOther"), wkswrkd = c(50, 50), occ = c("106", "106"))')

    data = robjects.r['svcensus'] 
    new_data = robjects.r('new_data')

    dsldLinRObject = dsldPyLinear(data, 'wageinc', 'gender', True, new_data)
    dsldPyLinearSummary(dsldLinRObject)

    dsldLinPyObject = dsldPyLinear(data, 'wageinc', 'gender', True, new_data, "Python")


    robjects.r('X_new <- data.frame(age = c(18, 60), educ = c("16", "16"), occ = c("106", "106"), wkswrkd = c(50, 50))')
    X_new = robjects.r('X_new')

    dsldDiffRObject = dsldPyDiffS(dsldLinRObject, X_new)
    print(dsldDiffRObject)

    dsldDiffPyObject = dsldPyDiffS(dsldLinRObject, X_new, "Python")
    print(dsldDiffPyObject)
}

