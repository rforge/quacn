\name{compactness}
\alias{compactness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compactness
%%  ~~function to do ... ~~
}
\description{This method calculates the compactness of a graph.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compactness(g, dist = NULL, wien = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{a graphNEL object
%%     ~~Describe \code{g} here~~
}
  \item{dist}{the Distance Matrix of the graph g (optional)
%%     ~~Describe \code{dist} here~~
}
  \item{wien}{the Wiener index of the graph g (optional)
%%     ~~Describe \code{wien} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (g, dist = NULL, wien = NULL) 
{
    if (class(g)[1] != "graphNEL") {
        stop("'g' must be a 'graphNEL' object")
    }
    if (is.null(dist)) {
        dist = distanceMatrix(g)
    }
    if (is.null(wien)) {
        wien <- wiener(g, dist)
    }
    nV <- numNodes(g)
    return(4 * wien/(nV * (nV - 1)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
