infoTheoreticGCM <- function(g, dist=NULL, coeff="lin", infofunct="sphere", lambda=1000, custCoeff=NULL){
  require("graph")
  allowed.coeff <-  c("lin","quad","exp","const","cust")
  allowed.functionals <-  c("sphere","pathlength","vertcent","degree")
  # check if g is a graphNEL object
  if(class(g)[1]!="graphNEL"){
    stop("'g' must be a 'graphNEL' object")
  }
  if(is.null(dist)){
    dist <- distanceMatrix(g)
  }
  ##check coeff
  if (!coeff %in% allowed.coeff){
    stop(paste("'",coeff,"' is no valid value for coeff -> valid valuse: ", paste(allowed.coeff,collapse=", "), sep=""))
  }
  ##assign coeff
  l <- max(dist)
    if (coeff==allowed.coeff[1]){#"lin"
    ci <- l:1
  }else if(coeff==allowed.coeff[2]){#"quad"
    ci <- (l:1)^2
  }else if(coeff==allowed.coeff[3]){#"exp"
    ci <- l*exp(0:(-l+1))
    #print(paste("YES: ",ci))
  }else if(coeff==allowed.coeff[4]){#"const"
    ci <- rep(1,l)
  }else if(coeff==allowed.coeff[5]){#"cust"
    ci <- custCoeff
  }
  ##check infofunct
  if (!infofunct %in% allowed.functionals){
    stop(paste("'",infofunct,"' is no valid value for infofunct -> valid valuse: ", paste(allowed.functionals,collapse=", "), sep=""))
  }
  ##calc functional
  if(infofunct==allowed.functionals[1]){#"sphere"
    fvi <- .functionalJSphere(g,dist=dist,ci=ci)
  }else  if(infofunct==allowed.functionals[2]){#"pathlength"
    fvi <- .functionalPathlength(g,dist=dist,ci=ci)
  }else  if(infofunct==allowed.functionals[3]){#"vertcent"
    fvi <- .functionalLocalProperty(g,dist=dist,ci=ci)
  }else  if(infofunct==allowed.functionals[4]){#"degree"
    fvi<-.functionalDegreeDegree(g,dist=dist,ci=ci)
  }
  ##calcualte the entrpy and the distance
  fvi.sum <- sum(fvi)
  pis <- fvi/fvi.sum
  itgcm <- list()
  itgcm[["entropy"]] <- (-sum(pis*log2(pis)))
  itgcm[["distance"]] <- (lambda*(log2(length(pis)) - itgcm[["entropy"]]))
  itgcm[["pis"]] <- pis
  itgcm[["fvis"]] <- fvi
  return (itgcm)
}  

.functionalJSphere <- function(g, dist, ci){
  # check if g is a graphNEL object
  if(class(g)[1]!="graphNEL"){
    stop("'g' must be a 'graphNEL' object")
  }
  #calculate Spheres
  nam <- nodes(g)
  Sj <- lapply(nam,function(n){
    table(dist[n,],exclude=0)
  })
  names(Sj) <- nam
  
  fvi <- sapply(Sj,function(s,pc=ci){
     sum(s*ci[1:length(s)])
  })
  names(fvi) <- nam
  return (fvi)

}

.functionalPathlength <- function(g, dist,ci){
  require("igraph")

  ig <- igraph.from.graphNEL(g)
  vs <- V(ig)
  lvs <- length(vs$name)
  fvi <- rep(0,lvs)
  nam <- nodes(g)
  #determine number of all possible shortest path
  for(n in 1:lvs){
    #f <- vs[vs$name==n] 
    asp <- get.all.shortest.paths(ig,from=(n-1))
    lvi <- table(sapply(asp,length)-1,exclude=0)
    fvi[n] <- sum(lvi*ci[1:length(lvi)])
  }
  names(fvi) <- nam
  return (fvi)
}

.functionalLocalProperty <- function(g, dist,ci){
  require("igraph")

  ig <- igraph.from.graphNEL(g)
  vs <- V(ig)
  lvs <- length(vs$name)
  fvi <- rep(0,lvs)
  nam <- nodes(g)
  #determine number of all possible shortest path
  for(n in 1:lvs){
    #f <- vs[vs$name==n] 
    asp <- get.all.shortest.paths(ig,from=(n-1))
    lvi <- table(sapply(asp,length)-1,exclude=0)
    tmp.sum <- sapply(1:max(as.numeric(names(lvi))),function(lpl){
      sum(1:lpl)
    })
    betai <- 1/tmp.sum*lvi
    fvi[n] <- sum(betai*ci[1:length(lvi)])
  }
  names(fvi) <- nam
  return (fvi)
}

.functionalDegreeDegree <- function(g, dist, ci){
  fdelta <- list()
  nodes <- 1:length(nodes(g))
  names(nodes) <- nodes(g)
  deg <- graph::degree(g)
  for(v in 1:numNodes(g)){
    ## calculate P (shortest paths)
    P=list(v) # calculate P for each vertex v (shortest paths)
    change = 1
    oldIndex = 1 # old index (beginning of the last iteration) (e.g. shortest path = 2)
    while(change != 0){
      change = 0; # has something changed (means is one list added to P nad how much)
      newIndex = length(P)+1 # index of the beginning iteration (e.g. shortest path = 3)
      # generate list with all paths
      while(oldIndex<newIndex){ # for every index betwwen old and new create paths for all neighbors
        lastVertex = P[[oldIndex]][length(P[[oldIndex]])] # last vertex in list of P[oldIndex]
        #
        lastVertex = names(nodes[lastVertex])
        #
        neighbors = grep(lastVertex,edges(g)) # find all neighbors of the last vertex in the path of P[oldIndex]
       
        for(n in 1:length(neighbors)){ # for every neigbor
          temp = c(P[oldIndex],neighbors[n]) # add neigbour to path of oldIndex
          for(t in 2: length(temp)){ # generate one list in temp[[1]]
            temp[[1]][length(temp[[1]])+1] <- temp[[t]][1]
          }
          if((length(temp[[1]]) - 1) == dist[v,neighbors[n]]){ # only if the length of temp is the shortest path
            change = change + 1 # somthing changed (a new path is added)
            P[length(P)+1] <- temp[1] # write new path in the end of P (even wrong paths are written into P (wrong paths are paths to verteses where shorter paths exist!))
          }
        }
        oldIndex = oldIndex+1 # go to the next index until new Index was reached
      }
      oldIndex = newIndex # the new index is th new old one
    }
    ## generate S ## 
    S = list(deg[v])
    names(S[[1]]) <- NULL
    for(i in 2:length(P)){
      S[[i]] <- deg[v]
      for(j in 2:length(P[[i]])){
        S[[i]][j] = deg[P[[i]][j]]
      }
      names(S[[i]]) <- NULL
    }
    ## calc fdelta
    Scopy = S
    Scopy[[1]] <- NULL
    fdelta[[v]] <- -1
    maxSize = length(Scopy[[length(Scopy)]])
    for(size in 2:maxSize){
      sum = 0
      for(i in length(Scopy):1){
        if(length(Scopy[[i]]) == size){
          for(j in 1:(size-1)){
            sum = sum + abs(Scopy[[i]][j]-Scopy[[i]][j+1])
          }
   
         # Scopy[[i]] <- NULL
        }
      }
      fdelta[[v]][size-1] = sum
    }
  }

  fvi=vector("numeric",length=length(fdelta))
  for(i in 1:length(fdelta)){
     fvi[i] = sum(fdelta[[i]] * ci[1:length(fdelta[[i]])])
  }
  return(fvi)
}
