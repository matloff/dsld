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
dsldFairTest(data, yName, sName, modelFunc, metricFunc, deweightPars=NULL, 
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
  \item{modelFunc}{
    a function in the dsld package that starts with dsldQeFair
  }
  \item{metricFunc}{
    made for functions in the fairness package with the same call signatures as
    dem_parity, prop_parity, equal_odds, etc.
    User may instead pass in a function with a 'model' parameter, which is a reference
    to the model object generated from the modelFunc e.g. 'function(model)\{\}'
  }
  \item{deweightPars}{
    values for de-emphasizing variables in a split, e.g. 'list(age=0.2,gender=0.5)'
    Unused variables in the deweightPars are set to 1, and then all values are
    scaled so the sum of all the deweights is 1.
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
  As of 9/4/23, the qeFunc arguement only works with dsldQeFairRF and qeRFranger
}
\value{
  a data frame containing the fairness metrics outputted from the model. The 
  last column, Misclass Rate, is independent of the rows.
}
\author{
  Jonathan Tran
}
\examples{
  data <- fairml::compas
  yName <- 'two_year_recid'
  sName <- 'race'
  
  # Demographic/Statistical parity
  dsld::dsldFairTest(data, yName, sName, dsld::dsldQeFairRF, fairness::prop_parity, nReps = 2)
  
  # S Correlation
  dsld::dsldFairTest(data, yName, sName, dsld::dsldQeFairRF, \(model) model$corrsens, nReps = 2)
}
