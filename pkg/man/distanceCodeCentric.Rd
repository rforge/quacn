\name{distanceCodeCentric}
\alias{distanceCodeCentric}
\title{Distance code centric index}
\description{This method calculates the distance code centric index of a graph.
}
\usage{
distanceCodeCentric(g, dist = NULL)
}
\arguments{
  \item{g}{the input graph as a graphNEL object
}
  \item{dist}{distance matrix of the graph g.  Will be automatically
    calculated if not supplied.
}
}

\value{This returns the distance code centric index of the graph as a
  double-precision floating point number.
}
\references{R. Todeschini and V. Consonni and R. Mannhold, Handbook of
  Molecular Descriptors, Wiley-VCH, Weinheim, Germany, 2002
}
\author{Lavanya Sivakumar, Michael Schutte
}

\examples{
library(graph)
library(RBGL)
set.seed(123)
g <- randomGraph(1:8, 1:5, 0.36, weights=FALSE)

# calculate Distance Matrix
dist <- distanceMatrix(g)

distanceCodeCentric(g, dist)
}
\keyword{ descriptors }
