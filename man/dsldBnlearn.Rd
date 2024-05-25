\name{dsldBnlearn}
\alias{dsldIamb}

\title{dsldBnlearn}

\description{
    Wrapper to functions in the \pkg{bnlearn} package.
}

\usage{
dsldIamb(data)
}

\arguments{
    \item{data}{
        Data frame
    }
}

\details{

Under stringent assumptions, fits a causal model to \code{data}.

}

\value{
    Object of type "bn" (\pkg{bnlearn} object). The generic \code{plot}
    function is callable on this object.
}

\author{
    N. Matloff
}

\examples{
   \donttest{
   data(svcensus)
   iambOut <- dsldIamb(svcensus)
   plot(iambOut)
   }
}

