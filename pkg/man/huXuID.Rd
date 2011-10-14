\name{huXuID}
\alias{huXuID}
\title{Hu-Xu ID number}
\description{This method calculates the Hu-Xu ID number.}
\usage{
huXuID(g, deg=NULL)
}
\arguments{
  \item{g}{a graph as a graphNEL object.  Each edge must have a "weight"
    data attribute containing one of the values 1 (single bond), 2
    (double bond), 3 (triple bond) or 1.5 (aromatic bond), and each
    vertex must have an "atom" data attribute specifying its atomic
    number.}
  \item{deg}{the degree of each node of g.  Will be automatically
    calculated if not supplied.}
}

\value{The resulting floating point value is computed from weighted path
sums based on the vertex degree, the atomic numbers, and edge
multiplicity.
}
\references{% TODO Hu and Xu, 1997
}
\author{Michael Schutte}

\examples{
set.seed(987)
g <- randomEGraph(LETTERS[1:10], 0.3)

edgeDataDefaults(g, "weight") <- 1
edgeData(g, "B", "I", "weight") <- 2
edgeData(g, "A", "F", "weight") <- 1.5

nodeDataDefaults(g, "atom") <- 6
nodeData(g, "A", "atom") <- 8

huXuID(g)
}
\keyword{ descriptors }