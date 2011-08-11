laplaceMatrix <- function(g) { 
  adj.mat <- adjacencyMatrix(g)
  D <- diag(rowSums(adj.mat, na.rm = FALSE, dims = 1))
  D - adj.mat
}
