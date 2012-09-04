.weightedMinPathSum <- function(g, weightfunc) {
  n <- nodes(g)
  ig <- igraph::igraph.from.graphNEL(g)

  all.sp <- sapply(n, function(v){
    sp <- igraph::get.all.shortest.paths(ig, v)$res
  })
  all.sp <- do.call(c, all.sp)
  sum(sapply(all.sp, function(ssp){
    if(length(ssp)==1)
      return(1)
    else {
      prod(sapply(2:length(ssp), function(i){ weightfunc(i, ssp[i-1], ssp[i])}))/2
    }
  }))
  
  sum(unlist((sapply(n, function(v){
    sp <- igraph::get.all.shortest.paths(ig, v)$res
    sum(sapply(sp, function(ssp){
      if(length(ssp)==1)
        return(1)
      else {
        prod(sapply(2:length(ssp), function(i){ weightfunc(i, ssp[i-1], ssp[i])}))/2
      }
    }))
  })))
  
  sum(sapply(as.character(n), function(v) {
    sp <- igraph::get.all.shortest.paths(ig, v)$res
    spn <- sapply(sp, function(xx){
      n[xx]
    })
    sum(sapply(spn, function(path) {
      if (length(path) == 1)
        return(1)
      else
        prod(sapply(1:(length(path)-1), function(i) {
          weightfunc(i, as.character(path[i]), as.character(path[i+1]))
        })) / 2
    }))
  }))
}
