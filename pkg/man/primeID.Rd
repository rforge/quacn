\name{primeID}
\alias{primeID}
\title{Prime ID number}
\description{This method calculates the prime ID number.}
\usage{
primeID(g, deg=NULL)
}
\arguments{
  \item{g}{a graph as a graphNEL object.}
  \item{deg}{the degree of each node of g.  Will be automatically
    calculated if not supplied.}
}

\value{This method works like the connectivityID method, but it assigns
distinct prime numbers to each different pair of vertex degrees
associated with an edge.
}
\references{% TODO Randi'c, 1986b
}
\author{Michael Schutte}

\examples{
set.seed(987)
g <- randomEGraph(LETTERS[1:10], 0.3)

primeID(g)
}
\keyword{ descriptors }
