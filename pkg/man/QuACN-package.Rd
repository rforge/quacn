\name{QuACN-package}
\alias{QuACN-package}
\alias{QuACN}
\docType{package}
\title{QuACN: Quantitative Analysis of Complex Networks
}
\description{This package offers a set of topological network measures to analyze complex Networks structurally.}

\author{
Laurin Mueller
}
\references{L.A.J. Mueller, K.G. Kugler, A. Dander, A. Graber, M. Dehmer, QuACN: An R Package for Analyzing Complex Biological networks quantitatively, Bioinformatics, Vol. 27 no. 1 2011, pages 140-141.}
\keyword{ package }

\examples{

rm(list=ls())
library("graph")
library("RBGL")
library("QuACN")

###
set.seed(123)
g <- randomGraph(1:8, 1:5, 0.36, weights=FALSE)
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
infoTheoreticGCM(g,mat.dist,coeff="lin",infofunct="sphere",lambda=1000)
infoTheoreticGCM(g,mat.dist,coeff="exp",infofunct="sphere",lambda=1000)
infoTheoreticGCM(g,mat.dist,coeff="const",infofunct="pathlength",lambda=4000)
infoTheoreticGCM(g,mat.dist,coeff="quad",infofunct="vertcent",lambda=1000)
infoTheoreticGCM(g,mat.dist,coeff="lin",infofunct="degree",lambda=1000)
##
eigenvalueBased(g,adjacencyMatrix,2)
eigenvalueBased(g,laplaceMatrix,2)
eigenvalueBased(g,distanceMatrix,2)
eigenvalueBased(g,distancePathMatrix,2)
eigenvalueBased(g,augmentedMatrix,2)
eigenvalueBased(g,extendedAdjacencyMatrix,2)
eigenvalueBased(g,vertConnectMatrix,2) 
eigenvalueBased(g,randomWalkMatrix,2)  
eigenvalueBased(g,weightStrucFuncMatrix_lin,2) 
eigenvalueBased(g,weightStrucFuncMatrix_exp,2)
}
