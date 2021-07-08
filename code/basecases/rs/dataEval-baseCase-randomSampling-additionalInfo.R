rm(list=ls())
# setwd("~/Documents/ds/sem4/thesis/trees-02-ranger/04-chipman")

library(dplyr)
library(ranger)
library(ggplot2)

# source('code/source/distance-matrices-02.R') # my own code
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/chipman.R')
source('code/source/helper-functions.R')

# load df (with factors) 
# load docN with N repetitions for 
## data splits and the ranger random forest build on the current train set
load('code/doc-500trees-10rep-5maxDepth-keepInbag.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

#install.packages('abind')
library(abind)
# example: turning a list with elements of the same length into a matrix
abind(docN[[1]]$ranger$inbag.counts, along=2) %>% dim


# turn inbag counts for all repetitions and each tree of the respective forest into a cube / 3dim array
(function(i) {abind(docN[[i]]$ranger$inbag.counts, along=2)}) %>%
  lapply(1:N,.) %>%
  abind(along=3) -> 
  ibc # inbag counts for all repetitions and trees of each forest

# dim(ibc) # check : obs 1:213, tree 1:500 , repetition 1:10
# these are the oob observations
# oob<-which(docN[[1]]$ranger$inbag.counts[[i]]==0)

# do the predictions and compare to the true values , using the acc function
myAcc <-function(i,tri) {
  
  which(ibc[,tri,i]==0) -> 
    oob
  
  predict(
    object=subforest(docN[[i]]$ranger$forest, tri)
    , data=df[docN[[i]]$train[oob],]
  ) %>%
    .$predictions %>%
    as.numeric ->
    preds
  
  return(acc(preds
             ,as.numeric(df[docN[[i]]$train[oob],'CAD'] ))
  )
}

acc.oob<-outer(X=1:N , Y=1:500, FUN=Vectorize(myAcc)) # X for the repetitions, Y for the trees
# res[1:2,1:3] # why the Numeric??
# dim(acc.oob) # check : N , rg$num.trees

# how it is used
i<-1 # a number in 1:N , a repetition
trindcs <- c(1,2,3) # randomly sampled trees
mean(acc.oob[i,trindcs]) # repetition 1, sub-forest of trees 1,2,3 , returning the mean of individual oob accuracy

# we will later be interested in the accuracy of the sub-forest (including ensemble meagic!)
# of these trees and how they perform on the validation set

p <- predict(subforest(docN[[i]]$ranger$forest, trindcs)
             ,df[docN[[i]]$val,]
             )$predictions

acc( p , df[docN[[i]]$val,'CAD'] )

# clean up
rm(p, i, trindcs)

####################################
#### baseline : random sampling ####
####################################

cluster.k<-c(3,5,7,11,13)
#cluster.k<-c(10,20,30,40,50)

rs<-data.frame(matrix(0,nrow=length(cluster.k)*N*10,ncol=6))
ct<-1 # index counter

for(i in 1:N){
  forest <- docN[[i]]$ranger$forest
  val<-docN[[i]]$val
  accff <- docN[[i]]$accuracy[[1]]
  dm<-docN[[i]]$distMatrices$d0
  preds<-predict(forest, data = df[val,], predict.all = TRUE)
  for(k in cluster.k){
    # mean accuracy of randomly sampled sub-forests
    for(j in 1:100){
      trindcs<-sample(1:forest$num.trees, k, replace = FALSE)
      
      rs[ct,]<- c(i # which data split, which full forest
                  ,k # number of clusters
                  , paste(trindcs[order(trindcs)],collapse=',') # add this to check if something is strange
                  , ( apsf(forest
                              , trindcs
                              , df[val,] ) / accff ) %>% round(4) # accuracy ratio on validation set
                  , mean(acc.oob[i,trindcs]) # accuracy on oob training data
                  #, mean(apply(preds$predictions[,trindcs] , 2, function(vec) acc(vec, as.numeric(df[val,'CAD']))))  %>% round(4) # mean accuracy of sampled trees
                  , sum(dm[trindcs,trindcs])/(k*(k-1))  %>% round(4) # mean distance of sampled trees , not using mean
                  )
      ct<-ct+1
    }    
  }
}
names(rs)<-c('forest','num.trees','sampled trees','accRatio','mean tree acc','mean tree dissimilarity')
# if we didn't have the string for the sampled trees, all variables would be numeric 
# and the following would be unneccessary
rs$forest<-as.numeric(rs$forest)
rs$num.trees<-as.numeric(rs$num.trees)
rs$accRatio<-as.numeric(rs$accRatio)
rs[,'mean tree acc'] <- as.numeric(rs[,'mean tree acc']) 
rs[,"mean tree dissimilarity"] <- as.numeric(rs[,"mean tree dissimilarity"])

lm1<-lm(accRatio~0+. , data=rs[,c(2,4,6)])
summary(lm1)

# this is good:
# the mean distance of trees (in the sample)
# significantly (and positively) contributes to the accuracy ratio
# as seen before, in the simple random sampling base case, the number of sampled trees also significantly (and positively) contributes to a high accuracy ratio
# this leads us to the base case of building a sub-forest from the cluster medoids

lm2<-lm(accRatio~0+. , data=rs[,c(2,4:6)])
summary(lm2)

# this is good, too:
# we use single tree accuracy on oob training data (it did not go into building the tree) 
# and find it pos correlated to the accuracy ratio (a scaled ensemble accuracy) on validation data.

####################################################
#### remains from developing the myAcc function ####
####################################################
#### will soon go ... ##############################
####################################################

# myPred was not used above but is a little less complex than the myAcc function
# do the predictions on oob training data
myPred<-function(i,tri) {
  oob<- which(ibc[,tri,i]==0)
  predict(
    object=subforest(docN[[i]]$ranger$forest, tri)
    , data=df[docN[[i]]$train[oob],]
    ) %>%
    .$predictions %>%
    as.numeric
}

# how myAcc works : using oob training observations , and predictions on oob observations , put them into acc(.,.)
# do an apply thing to get accuracies for all predictions of oob obs
oob<- which(ibc[,1,1]==0)
acc(myPred(1,1), as.numeric(df[docN[[1]]$train[oob],'CAD']))
