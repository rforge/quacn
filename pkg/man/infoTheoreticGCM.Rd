\name{infoTheoreticGCM}
\alias{infoTheoreticGCM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Shannon entropy with information functionals.
%%  ~~function to do ... ~~
}
\description{This method is mainly based on the principle of using
  information functionals (IF) to assign a probability value to each vertex
  of a network. Then it applies the results of the IF to SHANNON's
  entropy formula.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
infoTheoreticGCM(g, dist = NULL, coeff = "lin", infofunct = "sphere", lamda = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{a graphNEL object
%%     ~~Describe \code{g} here~~
}
  \item{dist}{the distance matrix of the graph
%%     ~~Describe \code{dist} here~~
}
  \item{coeff}{the setting of the weighting coefficient You can choose
    between "lin", "quad", "exp", "const" or "cust"
%%     ~~Describe \code{coeff} here~~
}
  \item{infofunct}{which information functional will be used. You can
    choose between "sphere", "pathlength" or "localprop"
%%     ~~Describe \code{infofunct} here~~
}
  \item{lamda}{scaling constant of the information functional
%%     ~~Describe \code{lamda} here~~
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
function (g, dist = NULL, coeff = "lin", infofunct = "sphere", 
    lamda = 1000) 
{
    allowed.coeff <- c("lin", "quad", "exp", "const", "cust")
    allowed.functionals <- c("sphere", "pathlength", "localprop")
    if (class(g)[1] != "graphNEL") {
        stop("'g' must be a 'graphNEL' object")
    }
    if (is.null(dist)) {
        dist <- distanceMatrix(g)
    }
    if (!coeff \%in\% allowed.coeff) {
        stop(paste("'", coeff, "' is no valid value for coeff -> valid valuse: ", 
            paste(allowed.coeff, collapse = ", "), sep = ""))
    }
    l <- max(dist)
    if (coeff == allowed.coeff[1]) {
        ci <- l:1
    }
    else if (coeff == allowed.coeff[2]) {
        ci <- (l:1)^2
    }
    else if (coeff == allowed.coeff[3]) {
        ci <- l * exp(0:(-l + 1))
    }
    else if (coeff == allowed.coeff[4]) {
        ci <- rep(1, l)
    }
    else if (coeff == allowed.coeff[5]) {
        ci <- custCoeff
    }
    if (!infofunct \%in\% allowed.functionals) {
        stop(paste("'", infofunct, "' is no valid value for infofunct -> valid valuse: ", 
            paste(allowed.functionals, collapse = ", "), sep = ""))
    }
    if (infofunct == allowed.functionals[1]) {
        fvi <- .functionalJSphere(g, dist = dist, ci = ci)
    }
    else if (infofunct == allowed.functionals[2]) {
        fvi <- .functionalPathlength(g, dist = dist, ci = ci)
    }
    else if (infofunct == allowed.functionals[3]) {
        fvi <- .functionalLocalProperty(g, dist = dist, ci = ci)
    }
    fvi.sum <- sum(fvi)
    pis <- fvi/fvi.sum
    itgcm <- list()
    itgcm[["entropy"]] <- (-sum(pis * log2(pis)))
    itgcm[["distance"]] <- (lamda * (log2(length(pis)) - itgcm[["entropy"]]))
    return(itgcm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
