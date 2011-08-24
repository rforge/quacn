\name{offDiagonal}
\alias{offDiagonal}
\title{Off-diagonal complexity index
}
\description{
This method calculates the off-diagonal complexity measure.
}
\usage{
offDiagonal(g, deg = NULL)
}
\arguments{
  \item{g}{
a graph as a graphNEL object.
}
  \item{deg}{
the degree of all nodes of g.  Will be calculated automatically if left
empty.
}
}
\details{
  This method calculates the off-diagonal complexity measure.
}
\value{
  It returns the off-diagonal complexity measure as a double-precision
  floating point value.
}
\references{J. Kim and T. Wilhelm. What is a complex graph? Physica A,
  387:2637-2652, 2008
}
\author{
TODO
}

\examples{
library(graph)

g <- randomEGraph(as.character(1:10), 0.6)

offDiagonal(g)

# alternatively:
deg <- graph::degree(g)
offDiagonal(g, deg)
}
\keyword{graphs}
