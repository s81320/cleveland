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
load('code/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

# accuracy of the full forest on the validation set
aff<-c(rep(0,N))
par(mfrow=c(1,1))
for(i in 1:N) aff[i]<-docN[[i]]$accuracy[1]
boxplot(aff, 
        main=paste('accuracy full forest, ',N,'repetitons\n(mean:', mean(aff) %>% round(3) , ', sd:', sd(aff) %>% round(3),')'))
summary(aff)
# focus on variation , measured by standard deviation
mean(aff) ; sd(aff)

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
  preds.v<-predict(forest, data = df[val,], predict.all = TRUE)
  preds.t<-predict(forest, data = df[test,], predict.all = TRUE)
  for(k in cluster.k){
    # mean accuracy of randomly sampled sub-forests
    for(j in 1:100){
      trindcs<-sample(1:forest$num.trees, k, replace = FALSE)
      
      rs[ct,]<- c(i # which data split, which full forest
                  ,k # number of clusters
                  , paste(trindcs[order(trindcs)],collapse=',') # add this to check if something is strange
                  , ( apsf(forest
                              , trindcs
                              , df[val,] ) / accff ) %>% round(4) # accuracy ratio
                  , mean(apply(preds$predictions[,trindcs] , 2, function(vec) acc(vec, as.numeric(df[val,'CAD']))))  %>% round(4) # mean accuracy of sampled trees
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

# this is not good:
# we use single tree accuracy on validation data and find it pos correlated to ensemble accuracy on validation data.
# we should use oob accuracy of individual trees and look for correlation with performance on validation set
