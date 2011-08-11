polynomial <- function(g, matrix_function, s=1){  
  require("graph")
  # check if g is a graphNEL object
  if(class(g)[1]!="graphNEL"){
    stop("'g' must be a 'graphNEL' object")
  }

  M <- do.call(matrix_function,list(g))

  EV <- as.double(abs(eigen(M,only.values=TRUE)$values));
  EVs <- EV^(1/s)
  sumEVs <- sum(EVs)
  pi<- EVs/sumEVs

  result <- list()

  ##Expression (2)
  if (any(sapply(pi, function(x) x == 0))) {
    warning("Matrix has a zero eigenvalue -> value was set to 0")
    result[["HMs"]] <- 0
  }
  else
    result[["HMs"]] <- (-1) * sum(pi*log2(pi))

  ##Expression (3)
  result[["SMs"]] <- sumEVs

  ##Expression (4)
  result[["ISMs"]] <- 1/sumEVs

  ##Expression (5)
  result[["PMs"]] <- prod(EVs)

  ##Expression (6)
  result[["IPMs"]] <- 1/result[["PMs"]]

  result
}
