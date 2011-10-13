huXuID <- function(g, deg=NULL) {
  if (class(g)[1] != "graphNEL")
    stop("'g' has to be a 'graphNEL' object")
  if (is.null(deg))
    deg <- graph::degree(g)

  n <- nodes(g)
  e <- edges(g)

  deg[deg == 0] <- 0.5
  Z <- sapply(n, function(v) as.integer(nodeData(g, v, "atom")))
  hxdeg <- deg * sqrt(Z)

  weightfunc <- function(i, from, to) {
    sqrt(
     (as.numeric(edgeData(g, from, to, "weight")) / (i + 1)) *
     (1 / (hxdeg[[from]] * hxdeg[[to]]))
    )
  }

  AID <- sapply(n, function(v) .weightedPathSum(e, v, weightfunc, unit=0, rmdupls=FALSE))

  sum(AID ^ 2)
}
