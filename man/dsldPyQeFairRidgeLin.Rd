\name{dsldPyQeFairRidgeLin}
\alias{dsldPyQeFairRidgeLin}

\title{dsldPyQeFairRidgeLin}

\description{ 
  This function is an interface to dsldQeFairRidgeLin. It uses ryp2 Python library to 
  handle to dsldQeFairRigdeLin from dsld R package. This function allows the computation
  of a linear model while allowing to tune certain parameters. 
  
  The return value of this function ia an s3 object. However, rpy2 handles the return 
  value as an rpy2 object(list of list).

  For more information on the R function itself, please refer to its man page, 
  which you can access through the command ?dsldQeFairRidgeLin
}

\usage{
  dsldPyQeFairRidgeLin(data, yName, sName = R_NULL, deweightPars = R_NULL, holdout = R_NULL)
}

\arguments{
    \item{data}{
        This is the data that must be used for computation. This could be a pandas dataframe 
        or R dataframe.
    }
    \item{yName}{
        Name of the variable to be predicted(Must be a str type).
    }
    \item{sName}{
          Name of the sensitive variable(Must be a str type). 
      }
    \item{deweightPars}{
        List of columns that need to be deweighted as well as their values. This parameter
        takes a dictionary as argument. Dictionary format: {'key': value} -> {str: float}
    }
    \item{holdout}{
        The size of holdout set, default to 1000 it the number of colmuns*0.1 is greater 
        than 1000.
    } 
}
\note{
    The usage of this function requires Python 3.10 and rpy2. It might not work on linux
    machines.
}    
\author{
    N. Matloff, S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\examples{  
    python
    from dsldQeFairRidgeLin_Py_R import dsldPyQeFairRidgeLin
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    resR = dsldPyQeFairRidgeLin(data, "wageinc", sName = "gender", deweightPars = {"occ":0.2})
    robjects.r.assign("resR", resR)
    robjects.r('print(resR$testAcc)')
    robjects.r('X_new  <- data.frame(age = c(18, 60), educ = c("zzzOther", "zzzOther"), occ = c("106", "106"), wkswrkd = c(50, 50), gender = c("male","male"))')
    robjects.r('rs <- predict(resR, X_new)')
    print(robjects.r('rs'))
}
        
