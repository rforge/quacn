\name{calculateDescriptors}
\alias{calculateDescriptors}
\title{Generalized descriptor calculation
}
\description{The method calculates multiple descriptors for a list of graphs.
}
\usage{
calculateDescriptors(graphs, ..., labels = FALSE)
}
\arguments{
  \item{graphs}{either a list of or a single graphNEL object.
}
  \item{...}{descriptors to calculate and arguments to pass, see `Details'.
}
  \item{labels}{whether or not the columns of the resulting data frame should
    be named using the getLabels() method.
}
}

\details{
  calculateDescriptors() calls each function specified in `...' for every
  graph in the given list and creates a data frame containing the calculated
  data.  You can specify the functions either as strings (such as
  \dQuote{totalAdjacency}) or using the numbers from the following table
  (e.g., 2001).  For convenience, the multiples of 1000 denote entire groups
  of descriptors.

  \tabular{rl}{
    1000 \tab --- all of 1xxx \cr
    1001 \tab wiener \cr
    1002 \tab harary \cr
    1003 \tab balabanJ \cr
    1004 \tab meanDistanceDeviation \cr
    1005 \tab compactness \cr
    1006 \tab productOfRowSums \cr
    1007 \tab hyperDistancePathIndex \cr
    2000 \tab --- all of 2xxx \cr
    2001 \tab totalAdjacency \cr
    2002 \tab zagreb1 \cr
    2003 \tab zagreb2 \cr
    2004 \tab modifiedZagreb \cr
    2005 \tab augmentedZagreb \cr
    2006 \tab variableZagreb \cr
    2007 \tab randic \cr
    2008 \tab complexityIndexB \cr
    2009 \tab normalizedEdgeComplexity \cr
    2010 \tab atomBondConnectivity \cr
    2011 \tab geometricArithmetic1 \cr
    2012 \tab geometricArithmetic2 \cr
    2013 \tab geometricArithmetic3 \cr
    2014 \tab narumiKatayama \cr
    3000 \tab --- all of 3xxx \cr
    3001 \tab topologicalInfoContent \cr
    3002 \tab bonchev1 \cr
    3003 \tab bonchev2 \cr
    3004 \tab bertz \cr
    3005 \tab radialCentric \cr
    3006 \tab vertexDegree \cr
    3007 \tab balabanlike1 \cr
    3008 \tab balabanlike2 \cr
    3009 \tab graphVertexComplexity \cr
    3010 \tab offdiagonal \cr
    3011 \tab spanningTreeSensitivity \cr
    3012 \tab distanceDegreeCentric \cr
    3013 \tab distanceCodeCentric \cr
    4000 \tab --- all of 4xxx \cr
    4001 \tab infoTheoreticGCM: vertcent, exp \cr
    4002 \tab infoTheoreticGCM: vertcent, lin \cr
    4003 \tab infoTheoreticGCM: sphere, exp \cr
    4004 \tab infoTheoreticGCM: sphere, lin \cr
    4005 \tab infoTheoreticGCM: pathlength, exp \cr
    4006 \tab infoTheoreticGCM: pathlength, lin \cr
    4007 \tab infoTheoreticGCM: degree, exp \cr
    4008 \tab infoTheoreticGCM: degree, lin \cr
    5000 \tab --- all of 5xxx \cr
    5001 \tab adjacencyMatrix: s=1 \cr
    5002 \tab adjacencyMatrix: s=2 \cr
    5003 \tab laplaceMatrix: s=1 \cr
    5004 \tab laplaceMatrix: s=2 \cr
    5005 \tab distanceMatrix: s=1 \cr
    5006 \tab distanceMatrix: s=2 \cr
    5007 \tab distancePathMatrix: s=1 \cr
    5008 \tab distancePathMatrix: s=2 \cr
    5009 \tab augmentedMatrix: s=1 \cr
    5010 \tab augmentedMatrix: s=2 \cr
    5011 \tab extendedAdjacencyMatrix: s=1 \cr
    5012 \tab extendedAdjacencyMatrix: s=2 \cr
    5013 \tab vertConnectMatrix: s=1 \cr
    5014 \tab vertConnectMatrix: s=2 \cr
    5015 \tab randomWalkMatrix: s=1 \cr
    5016 \tab randomWalkMatrix: s=2 \cr
    5017 \tab weightStrucFuncMatrix_lin: s=1 \cr
    5018 \tab weightStrucFuncMatrix_lin: s=2 \cr
    5019 \tab weightStrucFuncMatrix_exp: s=1 \cr
    5020 \tab weightStrucFuncMatrix_exp: s=2 \cr
    5021 \tab energy \cr
    5022 \tab laplacianEnergy \cr
    5023 \tab estrada \cr
    5024 \tab laplacianEstrada \cr
    5025 \tab spectralRadius \cr
    6000 \tab --- all of 6xxx \cr
    6001 \tab oneEdgeDeletedSubgraphComplexity \cr
    6002 \tab twoEdgesDeletedSubgraphComplexity \cr
    6003 \tab globalClusteringCoeff \cr
    7000 \tab --- all of 7xxx \cr
    7001 \tab mediumArticulation \cr
    7002 \tab efficiency \cr
    7003 \tab graphIndexComplexity
  }

  The arguments to these functions, such as the distance matrix or the
  list of vertex degrees, will be automatically supplied and reused.
  After each function (or group of functions), regardless of whether it
  was referred to by name or by its assigned number, you may optionally
  pass extra arguments as a list, but note that this will not override
  the calculated arguments.  If you wish to pass the same extra
  arguments to multiple functions, you can concatenate the latter to
  a vector.

  When functions are given by name, an \dQuote{@NAME} suffix can be used
  to give the column a different name in the output data frame.  This is
  needed when you want to calculate a descriptor more than once with
  varying arguments.
}

\value{A data frame where rows and columns represent the input graphs and
  the desired descriptors, respectively.  The rows will be named according
  to the graph list; the column names are the names of the called functions
  if labels is FALSE, otherwise the label expressions as returned by
  getLabels() (and found in the vignette).
}
\author{Michael Schutte
}

\examples{

library(RBGL)
set.seed(123)
g <- randomGraph(1:8, 1:5, 0.36)

calculateDescriptors(g, 1000, 2002, 2003)

calculateDescriptors(g, "randic", "offdiagonal", 7000, labels=TRUE)

# these will give the same results (although named differently):
calculateDescriptors(g, c(5011, 5013), list(s=3))
calculateDescriptors(g,
  "eigenvalueBased@ea", list(matrix_function="extendedAdjacencyMatrix", s=3),
  "eigenvalueBased@vc", list(matrix_function="vertConnectMatrix", s=3))
}
\keyword{ graph matrices }
