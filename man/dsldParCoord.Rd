\name{dsldParCoord}
\alias{dsld}

\title{dsldParCoord}
\description{
This is a wrapper for the freqparcoord function from the freqparcoord package (link in references).
}
\usage{
dsldParCoord <- function(data, m, columns, grpName)
}

\arguments{
  \item{data}{The data, in data frame or matrix form. If there are indicator}
  \item{m}{Number of lines to plot for each group. A negative value in conjunction with
method being "maxdens" indicates that the lowest-density lines are to be plotted.
If method is "locmax", m is forced to 1.}
  \item{columns}{Numbers of the columns of x to be displayed.}
  \item{grpName}{Column number for the grouping variable, if any (if none, all the data is treated
as a single group); vector or factor.}
}
\details{
dsldParCoord is a wrapper function of the function freqparcoord package in which,
freqparcoord creates a parallel coordinate graph.
The y-axis represents the distance from the mean of the column variable
}
\value{
The distance from the mean in terms of standard deviation
}
\references{
https://cran.r-project.org/web/packages/freqparcoord/index.html
}
\author{
Taha Abdullah, Billy Ouattara, Jonathan Tran, Brandon Zarate
}

\examples{
data(prgeng)
pe <- prgeng[,c(1,3,5,7:9)]
pe25 <- pe[pe$wageinc < 250000,]
pe25 <- makeFactor(pe25,c('educ','occ','sex'))
pe25disc <- discretize(pe25,nlevels=5)

dsldParCoord(pe25,10,c(1,5,6),'sex') # sample call to plot the graph
}

