##function to convert a graphNEL object to an igraph
##g: The graphNEL object to be converted
##keep.weights: a flag to indicate whether weights should be considered when performing the conversion (DEFAULT= FALSE)
.G2IG <- function(g, keep.weights=FALSE){
 if(any(unlist(edgeData(g, attr="weight")) >1) & keep.weights)
    warning("When using this conversion the edge weight information will be ignored")
 ig <- igraph::igraph.from.graphNEL(g, weight = keep.weights)
 return(ig)
}
