\name{dsldScatterPlot3D}
\alias{dsldScatterPlot3D}
\title{ScatterPlot3D in dsld}
\description{ The function, dlsdScatterPlot3D, creates a 3D scatterplot
that paints each unique group (a set of points) as a different color. These
groups are plotted against the 3 axes. For example: Looking at a dataset
that contains the unique groups catchers and shortstop. We can plot these two
groups against the 3 axes, age, height, weight. The function will produce a 3D
scatterplot of this data.}
\usage{dsldScatterPlot3D(data, axiscols, grpcol, angle)}

\arguments{
  \item{data}{
  The argument, data, is a data set that must contain a column of a non-numeric type  (i.e: shortstop, catcher, outfield...).
  This column will allow the function to create the groups necessary.
  The data set also requires 3 columns of numeric type (i.e: age, height, weight). These columns will allow the function to
  create the 3 axes necessary to plot graph.
  }
  \item{axiscols}{
  The argument, axiscols, must contain the indexes of the 3 columns that will be used to as the axes of the graph
  }
  \item{grpcol}{
  The argument, grpcol, must be a single index in which the values of this column must be non-numeric.
  }
  \item{angle}{
  The argument, angle, must be a numerica value that change the view-angle of the 3D scatterplot
  }
}

\value{
  It returns a visual of a 3D scatterPlot
}
\references{
Scaterrplot3d function:
\link{ https://cran.r-project.org/web/packages/scatterplot3d/index.html}
}
\author{
Jonathan Tran and Brandon Zarate }



\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}

