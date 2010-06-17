\name{productOfRowSums}
\alias{productOfRowSums}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Product of Row Sums
%%  ~~function to do ... ~~
}
\description{This method calculates the product of row sums.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
productOfRowSums(g, dist = NULL, log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
%%     ~~Describe \code{g} here~~
}
  \item{dist}{
%%     ~~Describe \code{dist} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
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
function (g, dist = NULL, log = FALSE) 
{
    if (class(g)[1] != "graphNEL") {
        stop("'g' must be a 'graphNEL' object")
    }
    if (is.null(dist)) {
        dist <- distanceMatrix(g)
    }
    if (log == FALSE) {
        return(prod(apply(dist, 1, sum)))
    }
    else {
        return(log2(prod(apply(dist, 1, sum))))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
