\name{informationBondIndex}
\alias{informationBondIndex}
\title{Information bond index}
\description{This method calculates the information bond index.}
\usage{
informationBondIndex(g)
}
\arguments{
  \item{g}{a graph as a graphNEL object.  Each edge must have a "weight"
    data attribute containing one of the values 1 (single bond), 2
    (double bond), 3 (triple bond) or 1.5 (aromatic bond).}
}

\value{The return value is the information bond index of the specified input
graph.  This measure is based on edge weights which are interpreted as the
bond multiplicity values.
}
\references{Dosmorov, S.V. Generation of Homogeneous Reaction Mechanism.
Kinetics and Catalysis, 1982}
\author{Michael Schutte}

\examples{
set.seed(987)
g <- randomEGraph(LETTERS[1:10], 0.3)
edgeDataDefaults(g, "weight") <- 1
edgeData(g, "E", "H", "weight") <- 2
edgeData(g, "I", "J", "weight") <- 2

informationBondIndex(g)
}
\keyword{ descriptors }
