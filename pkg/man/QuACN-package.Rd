\name{QuACN-package}
\alias{QuACN-package}
\alias{QuACN}
\docType{package}
\title{
This package contains topological network descriptors to analyze complex networks.
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab QuACN\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-08-01\cr
License: \tab LGPL\cr
LazyLoad: \tab yes\cr
}

}
\author{
Laurin Mueller, laurin.mueller@umit.at
Maintainer: Laurin Mueller <laurin.mueller@umit.at>
}
\references{D. Bonchev and D. H. Rouvray, Complexity in Chemistry, Biology, and Ecology, ser. Mathematical and
Computational Chemistry. Springer, 2005, New York, NY, USA.
9

M. Dehmer, K. Varmuza, S. Borgert, and F. Emmert-Streib,  On entropy-based molecular descriptors: Sta-
tistical analysis of real and synthetic chemical structures,  J. Chem. Inf. Model., vol. 49, pp. 1655 1663,
2009.

M. Dehmer, N. Barbarini, K. Varmuza, and A. Graber,  A large scale analysis of information-theoretic network
complexity measures using chemical structures,  PLoS ONE, vol. 4, no. 12, 2009.

R. Todeschini, V. Consonni, and R. Mannhold, Handbook of Molecular Descriptors.
weinheim, Germany.
Wiley-VCH, 2002,

M. Dehmer,  Information processing in complex networks: Graph entropy and information functionals, 
Applied Mathematics and Computation, vol. 201, pp. 82 94, 2008.

}
\keyword{ package }
\seealso{
}
\examples{

rm(list=ls())
library("graph")
library("Rgraphviz")
library("RBGL")
library("QuACN")

###
set.seed(123)
g <- randomGraph(1:8, 1:5, 0.36)
plot(g,"neato")
###

mat.adj <- adjacencyMatrix(g)
mat.dist <- distanceMatrix(g)
vec.degree <- graph::degree(g)
ska.dia <- diameter(g)
ska.dia <- diameter(g,mat.dist)
##

wien <- wiener(g)
wiener(g,mat.dist)
##
harary(g)
harary(g,mat.dist)
##
balabanJ(g)
balabanJ(g,mat.dist)
##
meanDistanceDeviation(g)
meanDistanceDeviation(g,mat.dist)
##
compactness(g)
compactness(g,mat.dist)
compactness(g,mat.dist,wiener(g,mat.dist))
##
productOfRowSums(g,log=FALSE)
productOfRowSums(g,log=TRUE)
productOfRowSums(g,mat.dist,log=FALSE)
productOfRowSums(g,mat.dist,log=TRUE)
##
hyperDistancePathIndex(g)
hyperDistancePathIndex(g,mat.dist)
hyperDistancePathIndex(g,mat.dist,wiener(g,mat.dist))
##
totalAdjacency(g)
totalAdjacency(g,mat.adj)
##
zagreb1(g)
zagreb1(g,vec.degree)
##
zagreb2(g)
zagreb2(g,vec.degree)
##
randic(g)
randic(g,vec.degree)
##
normalizedEdgeComplexity(g)
normalizedEdgeComplexity(g,totalAdjacency(g,mat.adj))
##
complexityIndexB(g)
complexityIndexB(g,mat.dist)
complexityIndexB(g,mat.dist,vec.degree)
##
degreeDistribution(g)
degreeDistribution(g,vec.degree)
##
numNodes(g)
numEdges(g)
##
localClusteringCoeff(g)
localClusteringCoeff(g,vec.degree)
##
topologicalInfoContent(g)
topologicalInfoContent(g,mat.dist)
topologicalInfoContent(g,mat.dist,vec.degree)
##
bertz(g)
bertz(g,mat.dist)
bertz(g,mat.dist,vec.degree)
##
bonchev1(g)
bonchev1(g,mat.dist)
##
bonchev2(g)
bonchev2(g,mat.dist)
bonchev2(g,mat.dist,wiener(g))
##
radialCentric(g)
radialCentric(g,mat.dist)
##
vertexDegree(g)
vertexDegree(g,vec.degree)
##
balabanlike1(g)
balabanlike1(g,mat.dist)
##
balabanlike2(g)
balabanlike2(g,mat.dist)
##
graphVertexComplexity(g)
graphVertexComplexity(g,mat.dist)
##
infoTheoreticGCM(g)
infoTheoreticGCM(g,mat.dist,coeff="lin",infofunct="sphere",lamda=1000)
infoTheoreticGCM(g,mat.dist,coeff="exp",infofunct="sphere",lamda=1000)
infoTheoreticGCM(g,mat.dist,coeff="const",infofunct="pathlength",lamda=4000)
infoTheoreticGCM(g,mat.dist,coeff="quad",infofunct="localprop",lamda=1000)
}
