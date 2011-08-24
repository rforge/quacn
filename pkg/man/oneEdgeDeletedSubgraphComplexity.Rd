\name{oneEdgeDeletedSubgraphComplexity}
\alias{oneEdgeDeletedSubgraphComplexity}
\title{Edge-deleted subgraph complexity and spanning tree sensitivity measures
}
\description{This method calculates several indices based on
  edge-deleted subgraphs.
}
\usage{
oneEdgeDeletedSubgraphComplexity(g, subgraphs = NULL)
}
\arguments{
  \item{g}{
a graph as a graphNEL object.
}
  \item{subgraphs}{
the edge-deleted subgraphs of g as a list of adjacency matrices, as
returned by edgeDeletedSubgraphs(g).  If this parameter is omitted, the
subgraphs will be calculated automatically.
}
}
\details{
  This method calculates the one-edge-deleted subgraph complexity with
  respect to the different number of spanning trees (C_1eST) and spectra
  of the Laplacian and signless Laplacian matrix (C_1eSpec), as well as
  the spanning tree sensitivity (STS) and spanning tree sensitivity
  differences (STSD) measures.
}
\value{
  The results are returned in a list with entries named C_1eST,
  C_1eSpec, STS and STSD.
}
\references{J. Kim and T. Wilhelm. What is a complex graph? Physica A,
  387:2637-2652, 2008
}
\author{
TODO
}

\examples{
library(graph)

g <- randomEGraph(as.character(1:12), 0.5)

result <- oneEdgeDeletedSubgraphComplexity(g)
result$C_1eST
result$C_1eSpec
result$STS
result$STSD
}
\keyword{graphs}
