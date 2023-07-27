\name{dsldDensityByS}
\alias{dsldDensityByS}
\title{Graphs densities of a response variable, grouped by a sensitive variable}
\usage{
dsldDensityByS(data, yName = NULL, sName = NULL)
}
\arguments{
\item{data}{A dataframe with 1 numerical column and a factor column}

\item{yName}{A name or index of the numerical column}

\item{sName}{A name or index of the factor column}
}
\description{
Graphs densities of a response variable, grouped by a sensitive variable. 
Has sliders to control the bandwidth of the density.
}
\author{Jonathan Tran}
\examples{
library(dsld)
data(svcensus)
dsld::dsldDensityByS(svcensus, "wageinc", "educ")
}