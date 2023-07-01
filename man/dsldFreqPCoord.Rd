\name{dsldFreqPCoord}
\alias{dsldFreqPCoord}

\title{dsldFreqPCoord}
\description{
    This is a wrapper for the freqparcoord function from the freqparcoord package (link in references).
}
\usage{
    dsldFreqPCoord <- function(data, m, columns, grpName, plot_filename = NULL)
}

\arguments{
    \item{data}{
        The data, in data frame or matrix form.
    }
    \item{m}{
        Number of lines to plot for each group. A negative value in conjunction with
        method being "maxdens" indicates that the lowest-density lines are to be plotted.
        If method is "locmax", m is forced to 1.
    }
    \item{columns}{
        Numbers of the columns of x to be displayed. This can be entered as either a string or integers of the 
        columns separated by commas but not a mixture of both.
    }
    \item{grpName}{
        Column number for the grouping variable, if any (if none, all the data is treated
        as a single group); vector or factor.
    }
    \item{plot_filename}{
        Name of the file that will hold the saved graph image. 
        If null, the graph will not be saved as an image -- it will be generated and displayed without being saved.
        If a filename is provided, the graph will not be displayed, only saved to a file with this filename.
    }
  }
\details{
    dsldFreqPCoord is a wrapper function of the function freqparcoord package in which,
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
    T. Abdullah, B. Ouattara, J. Tran, B. Zarate
}

\examples{
    % Need to fix the "run examples link". The error: "/Example/dsld not found"   appears
    # Example 1
    data(prgeng)
    pe <- prgeng[,c(1,3,5,7:9)]
    pe25 <- pe[pe$wageinc < 250000,]
    dsldFreqPCoord(pe25,10,c(1,5,6),'sex') # sample call to plot the graph
    # Example 2
    data(mlb)
    dsldFreqPCoord(mlb,5,4:6,7) # sample call to plot the graph
}
