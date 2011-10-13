bondOrderID <- function(g) {
  if (class(g)[1] != "graphNEL")
    stop("'g' has to be a 'graphNEL' object")

  n <- nodes(g)
  e <- edges(g)
  .weightedPathSum(e, NULL, function(i, from, to) as.numeric(edgeData(g, from, to, "weight")))
}
