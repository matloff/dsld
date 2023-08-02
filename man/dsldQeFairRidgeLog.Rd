\name{dsldQeFairRidgeLog}
\alias{dsldQeFairRidgeLog}

\title{DSLDQeFairRidgeLog}

\description{ 
  To-do
}

\usage{
  dsldQeFairRidgeLog <- function(data,yName,deweightPars,sName = NULL, yesYVal = NULL, holdout=floor(min(1000,0.1*nrow(data))))
}

\arguments{
    \item{data}{
        to-do
    }
    \item{yName}{
        A numeric vector, the response variable.
    }
    \item{deweightPars}{
        to-do
    }
    \item{sName}{
        to-do
    }
    \item{yesYVal}{
        to-decorrelated
    }
    \item{holdout}{
        to-decorrelated
    } 
}

\author{
    N. Matloff, S. Martha, A. Mittal, B. Ouattara, B. Zarate
}

\details{
    To-do
}

\examples{
  data(law.school.admissions)                      
  drop <- c('fulltime','cluster')
  law.school.admissions <- law.school.admissions[, !(names(law.school.admissions) %in% drop)]
  law.school.admissions$bar <- as.integer(as.logical(law.school.admissions$bar))
  law.school.admissions$bar <- as.factor(law.school.admissions$bar)
  z <- dsldQeFairRidgeLog(data=law.school.admissions,yName='bar',deweightPars=list(fam_inc=0.2),sName='gender', yesYVal = '1') # causes Error 
}
