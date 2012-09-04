.weightedMinPathSum <- function(g, weightfunc) {
  n <- nodes(g)
  ig <- igraph::igraph.from.graphNEL(g)

  sum(sapply(1:length(n), function(v) {
    sp <- igraph::get.all.shortest.paths(ig, v)$res
    spn <- sapply(sp, function(xx){
      n[xx]
    })
    sum(sapply(spn, function(path) {
      if (length(path) == 1)
        return(1)
      else
        prod(sapply(1:(length(path) - 1), function(i) {
          weightfunc(i, path[i], path[i+1])
        })) / 2
    }))
  }))
}
