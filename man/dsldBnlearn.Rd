\name{dsldBnlearn}
\alias{dsldIamb}

\title{dsldBnlearn}

\description{
    Wrappers for functions in the \pkg{bnlearn} package. (Just 
    (Presently, just \code{iamb}.)
}

\usage{
dsldIamb(data)
}

\arguments{
    \item{data}{
        Data frame.
    }
}

\details{

Under very stringent assumptions, \code{dsldIamb} performs causal
discovery, i.e. fits a causal model to \code{data}.

}

\value{
    Object of class 'bn' (\pkg{bnlearn} object). The generic \code{plot}
    function is callable on this object.
}

\author{
    N. Matloff
}

\examples{
   \donttest{
   data(svcensus)
   # iamb does not accept integer data
   svcensus$wkswrkd <- as.numeric(svcensus$wkswrkd)
   svcensus$wageinc <- as.numeric(svcensus$wageinc)
   iambOut <- dsldIamb(svcensus)
   plot(iambOut)
   }
}

