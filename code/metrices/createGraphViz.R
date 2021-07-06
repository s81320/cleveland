# for a full binary tree with given split variables 
# create the graphViz code to visualize the tree

rm(list=ls())

library(dplyr)
library(ranger)

#source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/distance-matrices-03-scaled01.R')

load('code/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)

rg <- docN[[1]]$ranger
ti<-treeInfo(rg,1)

arrow <- function(a,b) paste("\"",a,"\"->\"",b,"\"",sep='')
node  <- function(n,l,c) paste("\"",n,"\" [label=\"", l ,"\" color= ", c , "]", sep='')


triViz <- function(tri, filename="tree.gv"){
  # from the ranger treeInfo create code for a vsualization with graphViz
  # save it to a default file tree.gv at the top / root of the project
  
  lines1 <- rep("#", nrow(ti))
  lines2 <- rep('#', 3*table(ti$terminal)["TRUE"] )

  ct<-0

  for(i in 1:nrow(ti)){
    if(ti[i,"terminal"]){
      lines1[i]<-node(ti[i,"nodeID"],ti[i,"prediction"], ifelse(ti[i,"prediction"]=='Yes','red','green'))
    }else{
      lines2[ct+1] <- node(ti[i,"nodeID"],ti[i,"splitvarID"],'black')
      lines2[ct+2] <- arrow(ti[i,"nodeID"], ti[i,"leftChild"])
      lines2[ct+3] <- arrow(ti[i,"nodeID"], ti[i,"rightChild"])
      ct <- ct+3
    }
  }

  fileConn<-file(filename)
  writeLines(c("digraph fbt {", "forcelabels=true ;" ,lines2, lines1,"}"), con=fileConn)
  close(fileConn)
}
