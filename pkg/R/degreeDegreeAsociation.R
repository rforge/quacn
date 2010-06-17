degreeDegreeAsociation <- function(g, distMat=NULL, deg=NULL, alpha, c){
  # check objects
  if(class(g)[1]!="graphNEL"){
    stop("'g' must be a 'graphNEL' object")
  }
  if(!is.numeric(c)){
    stop("'c' must be a 'numeric'")
  }
  if(!is.numeric(alpha)){
    stop("'alpha' must be a 'numeric'")
  }
  # some parameters
  if(is.null(distMat)){
    distMat = distanceMatrix(g)
  }
  dia = diameter(g,distMat)
  if(length(c)!=dia){
    stop("'c' must have the same length as the diameter of the graph")
  }
  if(is.null(deg)){
    deg = graph::degree(g)
  }

  fdelta = list(NULL)
  #
  nodes = 1:length(nodes(g))
  names(nodes) = nodes(g)
  #
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
          if((length(temp[[1]]) - 1) == distMat[v,neighbors[n]]){ # only if the length of temp is the shortest path
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

  ## calculate index
  value = rep(0,length(fdelta))
  for(i in 1:length(fdelta)){
    for(j in 1:length(fdelta[[i]])){
      value[i] = value[i] + fdelta[[i]][j] * c[j]
    }
  }
  value = alpha^value
  Dg = sum(value)
  DgValue = value/Dg
  index = 0
  for(i in 1:length(DgValue)){
    index = index - DgValue[i]*log2(DgValue[i])
  }
  return (index)
}
