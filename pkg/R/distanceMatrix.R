distanceMatrix <- function(g) {
  #print("Distance...")
  # check if g is a graphNEL object
  if(class(g)[1]!="graphNEL"){
    stop("'g' has to be a 'graphNEL' object")
  }
  johnson.all.pairs.sp(g)
}
