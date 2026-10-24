\name{dsldBnlearn}
\alias{dsldIamb}

\title{dsldBnlearn}

\description{
    Wrappers for functions in the \pkg{bnlearn} package. (Presently, just \code{iamb}.)
}

\usage{
dsldIamb(data)
}

\arguments{
    \item{data}{
        Data frame with complete, finite observations. Integer columns are
        converted to numeric and character columns to factors for bnlearn.
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
   if (requireNamespace("bnlearn", quietly = TRUE)) {
   data(svcensus)
   set.seed(2026)
   svcensus <- svcensus[sample.int(nrow(svcensus), 1000), ]
   iambOut <- dsldIamb(svcensus)
   plot(iambOut)
   }
   }
}

