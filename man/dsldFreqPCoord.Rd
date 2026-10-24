\name{dsldFreqPCoord}
\alias{dsldFreqPCoord}

\title{dsldFreqPCoord}

\description{
    Wrapper for the \code{freqparcoord} function from the \pkg{freqparcoord} 
    package.
}
\usage{
dsldFreqPCoord(data, m, sName = NULL, method
    = "maxdens", faceting = "vert", k = 50, klm = 5 * k, keepidxs = NULL, 
    plotidxs = FALSE, cls = NULL, plot_filename = NULL)
}

\arguments{
    \item{data}{
        Data frame or matrix.
    }
    \item{m}{
        Number of lines to plot for each group. A negative value in conjunction 
        with the method \code{maxdens} indicates that the 
        lowest-density lines are to be plotted.  If method is \code{locmax},
        then \code{m} is forced to 1.
    }
    \item{sName}{
        Name of the grouping column, if any. It is excluded from the plotted
        axes. With \code{NULL}, all rows form one group. Grouping is ignored
        when \code{method = "locmax"}.
    }
    \item{method}{
        What to display: 'maxdens' for plotting the most
        (or least) typical lines, 'locmax' for cluster hunting, or 
        'randsamp' for plotting a random sample of lines.
    }
    \item{faceting}{
        How to display groups, if present.  Use 'vert' for
        vertical stacking of group plots, 'horiz' for horizontal ones, or
        'none' to draw all lines in one plot, color-coding by group.
    }
    \item{k}{
        Number of nearest neighbors to use for density estimation.
    }
    \item{klm}{
        If method is "locmax", number of nearest neighbors to 
        use for finding local maxima for cluster hunting. Generally needs
        to be much larger than \code{k}, to avoid "noise fitting."
    }
    \item{keepidxs}{
        Optional numeric column index in the encoded data passed to the
        backend, used to order retained rows. If not \code{NULL}, the plotted
        row indices are returned in \code{idxs}, and the encoded rows in
        \code{xdisp}, ordered by this column. Factor expansion and omission
        of globally constant predictors can change the column indices.
    }
    \item{plotidxs}{
        If TRUE, lines in the display will be annotated 
        with their case numbers, i.e. their row numbers within \code{data}.  
        Use only with small values of \code{m}, as overplotting may occur.
    }
    \item{cls}{
        Cluster, if any (see the \code{parallel} package) for
        parallel computation.
    }
    \item{plot_filename}{
        Name of the file that will hold the saved graph image. 
        If NULL, the graph will be generated and displayed without being saved.
        
        If a filename is provided, the graph will not be displayed, only
        saved.
    }
}

\details{
    The \code{dsldFreqPCoord} function wraps \code{freqparcoord},
    which uses a frequency-based parallel coordinates method to 
    visualize multiple variables simultaneously in graph form.
    
    This is done by plotting either the "most typical" or "least typical"
    (i.e. highest or lowest estimated multivariate density values respectively)
    cases to discern relations between variables.  
    
    Unused factor levels are dropped before encoding. Constant predictor
    columns are omitted with a warning because they cannot be scaled.
    The density estimator also scales within each plotted group. Encoded axes
    that are constant within any such group are omitted from the plot and
    density calculation with an explicit warning. Inspect those attributes
    separately, for example with \code{dsldFrequencyByS}: a deterministic
    group relationship can be substantively important even though this density
    estimator cannot use it. At least one axis must vary within every group.

    The Y-axis represents the centered and scaled values of the columns.
}

\value{
    Object of type 'gg' (\pkg{ggplot2} object), with components \code{idxs}
    and \code{xdisp} added if \code{keepidxs} is not NULL (see argument
    \code{keepidxs} above).
}

\references{
    https://cran.r-project.org/web/packages/freqparcoord/index.html
}
\author{
    N. Matloff, T. Abdullah, B. Ouattara, J. Tran, B. Zarate
}

\examples{
data(lsa)
lsa1 <- lsa[,c('fam_inc','ugpa','gender','lsat','race1')]
dsldFreqPCoord(lsa1,75,'race1')
# a number of interesting trends among the most "typical" law students in the
# dataset: remarkably little variation among typical
# African-Americans; typical Hispanic men have low GPAs, poor LSAT
# scores there is more variation; typical Asian and Black students were
# female; Asians and Hispanics have the most variation in family income
# background
}

