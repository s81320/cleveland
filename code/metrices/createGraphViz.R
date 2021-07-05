# for a full binary tree with given split variables 
# create the graphViz code to visualize the tree

# information on classification in terminal nodes missing

rm(list=ls())

library(dplyr)
library(ranger)

#source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/distance-matrices-03-scaled01.R')

load('code/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)

rg <- docN[[1]]$ranger
#sf <- subforest(rg$forest, c(1,2,3))
t1 <- rt2fbt(rg$forest, trindx=3)
t1

arrow <- function(a,b) paste("\"",a,"\"->\"",b,"\"",sep='')
#node  <- function(n,l,c) paste("\"",n,"\" [label=\"", l ,"\" color= ", c , "]", sep='')
node  <- function(n,l) paste("\"",n,"\" [label=\"", l ,"\" ]", sep='')

lines1<- rep("#",62)
for(i in 1:31){
  if(t1[i]!=0){
    if(2*i<32 && t1[2*i]!=0) lines1[i] <- arrow(i,2*i)
  }
}
for(i in 1:31){
  if(t1[i]!=0){
    if(2*i+1<32 && t1[2*i+1]!=0) lines1[31+i] <- arrow(i,2*i+1)
  }
}

lines2 <- rep("#",31)
for(i in 1:31){
  if(t1[i]!=0){
    lines2[i]<- node(i,t1[i]-1)}
}
#lines2

fileConn<-file("a_tree.gv")
writeLines(c("digraph fbt {", "forcelabels=true ;" ,lines2, lines1,"}"), con=fileConn)
close(fileConn)
