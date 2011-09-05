rm(list=ls())
library("graph")
importSDF <- function(filename){
  sFile <- file(paste(filename,"sdf",sep="."),"r")
  #sFile <- filename
  lines <- readLines(sFile)
  i <- 1
  gn <- 1
  graphs <- list()
  ngs <-  length(lines[lines=="$$$$"])
  #ngs=23900
  while(gn<=ngs){
    if(gn%%100==0){
      print(paste(gn,"of",ngs,"line: ",i,sep=" "))
    }
    gname <- lines[i]
#     tmp <- unlist(strsplit(lines[i+3]," "))
#     tmp <- tmp[tmp!=""]
#     nnod <- as.integer(tmp[1])
#     ned <- as.integer(tmp[2])
    #create graph with nodes
     tmp <- lines[i+3]
     nnod <- as.integer(substr(tmp,1,3))
     ned <- as.integer(substr(tmp,4,6))
     gg = new("graphNEL", nodes=as.character(1:nnod))
    #create edges
    for(j in 1:ned){
#       tmp <- unlist(strsplit(lines[i+3+nnod+j]," "))
#       tmp <- tmp[tmp!=""]
#       a <- tmp[1]
#       e <- tmp[2]
      tmp <- lines[i+3+nnod+j]
      a <- as.character(as.integer(substr(tmp,1,3)))
      e <- as.character(as.integer(substr(tmp,4,6)))
      gg <- addEdge(a,e,gg,1)
    }
    graphs[[paste(gname,"-GN-",gn,sep="")]] <- gg

    i <- i + 3 + nnod + j #+2
    while(!lines[i]=="$$$$"){
      i <- i + 1
    }
    i <- i+1
    gn <- gn + 1
  }
  close(sFile)
  return (graphs)
}

file <- "/mnt/projects/DATA/MalariaCompoundsChembl/GSK/chemblntd_gsk"
graphs <- importSDF(filename)
save(graphs,file=paste(file,"RData",sep="."))

file <- "/mnt/projects/DATA/MalariaCompoundsChembl/GSK/chemblntd_all"
graphs <- importSDF(file)
save(graphs,file=paste(file,"RData",sep="."))


filename <- "~/Downloads/chemblntd_all"
