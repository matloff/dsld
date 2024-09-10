\name{dsldFairUtilTrade}
\alias{dsldFairUtilTrade}
\title{dsldFairUtilTrade}

\description{ 
Exploration of the Fairness-Utility Tradeoff.  Finds predictive accuracy
and correlation between S and predicted Y.
}

\usage{
dsldFairUtilTrade(data,yName,sName,dsldFtnName,
   unfairness=NULL,deweightPars=NULL,yesYVal=NULL,yesSVal=NULL,
   corrType='kendall', holdout = floor(min(1000, 0.1 * nrow(data))))
}

\arguments{
    \item{data}{
        Data frame. 
    }
    \item{yName}{
        Name of the response variable Y column. Y must be numeric or
        binary (two-level R factor).
    }
    \item{sName}{
        Name of the sensitive attribute S column. S must be numeric or
        binary (two-level R factor).
    }
    \item{dsldFtnName}{
        Quoted name of one of the \pkg{fairML} or EDF functions.   
    }
    \item{unfairness}{
        Nonnull for the \pkg{fairML} functions.
    }
    \item{deweightPars}{
        Nonnull for the EDF functions.
    }
    \item{yesYVal}{
        Y value to be treated as Y = 1 for binary Y.
    }
    \item{yesSVal}{
        S value to be treated as S = 1 for binary S.
    }

    \item{corrType}{
        Either 'kendall' or 'probs'.
    }

    \item{holdout}{
        Size of holdout set.
    }
}

\author{
    N. Matloff
}

\examples{  

data(svcensus)
dsldFairUtilTrade(svcensus,'wageinc','gender','dsldFrrm',0.2,yesSVal='male')
data(lsa)
race1 <- lsa$race1
lsabw <- lsa[race1 == 'black' | race1 == 'white',]
# need to get rid of excess levels
race1 <- lsabw$race1
race1 <- as.character(race1)
lsabw$race1 <- as.factor(race1)
dsldFairUtilTrade(lsabw,'bar','race1','dsldQeFairRidgeLog',
   deweightPars=list(fam_inc=0.1),yesYVal='TRUE',yesSVal='white')

}

\details{

Tool for exploring tradeoff between utility (predictive accuracy, Mean
Absolute Prediction Error or overall probability of misclassification)
and fairness. Roughly speaking, the latter is defined as the strength of
relation between S and predicted Y (the smaller, the better). The main
issue is definition of "relation" in the case of binary Y or S:

In the 'kendall' case, binary predicted Y or S is recoded to 1s and 0s,
and Kendall correlation is used. In the 'probs' case, binary Y or S is
replaced by P(Y = 1 | X) and P(S = 1 | X); squared Pearson correlation
is then computed.

}

\value{

A two-component vector, consisting of predictive accuracy and strength
of relation between S and predicted Y.
}

