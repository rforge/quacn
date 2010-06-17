\name{degreeDegreeAsociation}
\alias{degreeDegreeAsociation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Degree degere Asociaton index
%%  ~~function to do ... ~~
}
\description{This method calculates the degree degere asociaton index of
  a given graph.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
degreeDegreeAsociation(g, distMat = NULL, deg = NULL, alpha, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
%%     ~~Describe \code{g} here~~
}
  \item{distMat}{
%%     ~~Describe \code{distMat} here~~
}
  \item{deg}{
%%     ~~Describe \code{deg} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{c}{
%%     ~~Describe \code{c} here~~
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
function (g, distMat = NULL, deg = NULL, alpha, c) 
{
    if (class(g)[1] != "graphNEL") {
        stop("'g' must be a 'graphNEL' object")
    }
    if (!is.numeric(c)) {
        stop("'c' must be a 'numeric'")
    }
    if (!is.numeric(alpha)) {
        stop("'alpha' must be a 'numeric'")
    }
    if (is.null(distMat)) {
        distMat = distanceMatrix(g)
    }
    dia = diameter(g, distMat)
    if (length(c) != dia) {
        stop("'c' must have the same length as the diameter of the graph")
    }
    if (is.null(deg)) {
        deg = graph::degree(g)
    }
    fdelta = list(NULL)
    nodes = 1:length(nodes(g))
    names(nodes) = nodes(g)
    for (v in 1:numNodes(g)) {
        P = list(v)
        change = 1
        oldIndex = 1
        while (change != 0) {
            change = 0
            newIndex = length(P) + 1
            while (oldIndex < newIndex) {
                lastVertex = P[[oldIndex]][length(P[[oldIndex]])]
                lastVertex = names(nodes[lastVertex])
                neighbors = grep(lastVertex, edges(g))
                for (n in 1:length(neighbors)) {
                  temp = c(P[oldIndex], neighbors[n])
                  for (t in 2:length(temp)) {
                    temp[[1]][length(temp[[1]]) + 1] <- temp[[t]][1]
                  }
                  if ((length(temp[[1]]) - 1) == distMat[v, neighbors[n]]) {
                    change = change + 1
                    P[length(P) + 1] <- temp[1]
                  }
                }
                oldIndex = oldIndex + 1
            }
            oldIndex = newIndex
        }
        S = list(deg[v])
        names(S[[1]]) <- NULL
        for (i in 2:length(P)) {
            S[[i]] <- deg[v]
            for (j in 2:length(P[[i]])) {
                S[[i]][j] = deg[P[[i]][j]]
            }
            names(S[[i]]) <- NULL
        }
        Scopy = S
        Scopy[[1]] <- NULL
        fdelta[[v]] <- -1
        maxSize = length(Scopy[[length(Scopy)]])
        for (size in 2:maxSize) {
            sum = 0
            for (i in length(Scopy):1) {
                if (length(Scopy[[i]]) == size) {
                  for (j in 1:(size - 1)) {
                    sum = sum + abs(Scopy[[i]][j] - Scopy[[i]][j + 
                      1])
                  }
                }
            }
            fdelta[[v]][size - 1] = sum
        }
    }
    value = rep(0, length(fdelta))
    for (i in 1:length(fdelta)) {
        for (j in 1:length(fdelta[[i]])) {
            value[i] = value[i] + fdelta[[i]][j] * c[j]
        }
    }
    value = alpha^value
    Dg = sum(value)
    DgValue = value/Dg
    index = 0
    for (i in 1:length(DgValue)) {
        index = index - DgValue[i] * log2(DgValue[i])
    }
    return(index)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
