\name{dsldFairTest}
\alias{dsldFairTest}
\title{
  Perform fairness measurements on dsldQeFair models
}
\description{
  Takes a model generation function, a fairness metric function, and outputs
  the fairness measurement of the model.
}
\usage{
dsldFairTest(data, yName, sName, qeFunc, fairFunc, deweightPars=NULL, 
  cutoff = .5, nReps = 1, testProportion = 0.3)
}
\arguments{
  \item{data}{
    data frame
  }
  \item{yName}{
    column name indicating a binary outcome variable
  }
  \item{sName}{
    column name of the sensitive variable (factor)
  }
  \item{qeFunc}{
    a function in the dsld package that starts with dsldQeFair
  }
  \item{fairFunc}{
    a function in the fairness package.
  }
  \item{deweightPars}{
    values for de-emphasizing variables in a split, e.g. 'list(age=0.2,gender=0.5)'
    Default set to a list of all the column names set to 1
  }
  \item{cutoff}{
    cutoff to generate predicted outcomes from predicted probabilities. 
    Default set to 0.5.
  }
  \item{nReps}{
    how many times to repeat the calculation before taking the average of all of them. 
    Default at 1.
  }
  \item{testProportion}{
    how much of the data to use as the testing set. The rest is used to train the model.
  }
}
\details{
  As of August, the qeFunc arguement only works with dsldQeFairRF
}
\value{
  a data frame containing the fairness metrics outputted from the model
}
\author{
  Jonathan Tran
}
\examples{
  data <- fairml::compas
  yName <- 'two_year_recid'
  sName <- 'race'
  
  dsldFairTest(data, yName, sName, dsld::dsldQeFairRF, fairness::prop_parity, nReps = 2)
  
  deweightPars = list(decile_score=0, priors_count=0)
  dsldFairTest(data, yName, sName, dsld::dsldQeFairRF, fairness::prop_parity, nReps = 2, deweightPars = deweightPars)
}
