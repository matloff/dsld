\name{dsldPyTakeALookAround}
\alias{dsldPyTakeALookAround}

\title{DSLDPyTakeALookAround}

\description{
    This function, meant for Python users, uses the rpy2 library to call the R function dsldTakeALookAround() from within a Python environment. 

    The function takes in a list of Python arguments corresponding to the arguments to the R function. These Python arguments are converted to R arguments     that are then passed into the R function. This way, the Python user can call the function similar to how an R user would call the function. 
    
    For more information on the R function itself, please refer to its man page, which you can access through the command ?dsldTakeALookAround
}

\usage{
    dsldTakeALookAround <- function(data,yName,sName, maxFeatureSetSize = (ncol(data) - 2))
}

\arguments{
    \item{data}{dataset in dataframe form}
    \item{yName}{as in qeML functions, the response variable}
    \item{sName}{name of the sensitive variable, an R factor}
    \item{maxFeatureSetSize}{maximum number of combinations of features to be included in the dataframe; 
    default argument set as: maxFeatureSetSize = (ncol(data) - 2)}
}

\author{
    T. Abdullah, S. Martha, B. Ouattara, B. Zarate
}

\examples{
  #   The dsldTakeALookAround function aims to provide a seamless experience, requiring minimal effort to utilize its
  #   capabilities. Once the package is successfully downloaded and installed, the function can be leveraged by simply 
  #   employing similar commands shown in the examples.
      
  #   The examples are conducted using the 'pef' dataset. 
  
  dsldPyTakeALookAround(pef, 'wageinc','sex',4) # run w maxFeatureSetSize to get all possible combinations
  dsldPyTakeALookAround(pef, 'wageinc','sex') # uses default maxFeatureSetSize
  dsldPyTakeALookAround(pef, 'wageinc','sex',1) # run w smaller maxFeatureSetSize
  dsldPyTakeALookAround(pef, 'wageinc','sex',5) # returns error if maxFeatureSetSize too large
}

