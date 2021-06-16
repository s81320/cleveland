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
load('code/chipman/04/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
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

rs<-data.frame()
for(i in 1:N){
  forest <- docN[[i]]$ranger$forest
  val<-docN[[i]]$val
  accff <- docN[[i]]$accuracy[[1]]
  for(k in cluster.k){
    # mean accuracy of randomly sampled sub-forests
    for(j in 1:100){
      new.row<- c( i
                   , k
                   , ( apsf(forest
                            , sample(1:forest$num.trees, k, replace = FALSE)
                            , df[val,]
                   ) / accff ) %>% round(4)
      )
      rs<-rbind(rs,new.row)
    }    
  }
}
names(rs)<-c('forest','num.trees','accRatio')

# rs$num.trees<-as.factor(rs$num.trees)

stats<-rs %>% group_by(num.trees) %>% summarise(m=mean(accRatio), s=sd(accRatio)) %>% data.frame()
names(stats)<- c('number of trees','mean accuracy ratio','sd accuracy ratio')

baseCase<- list(rs=rs , stats=stats)
rm(rs,stats)

ggplot(baseCase$rs, aes(x=as.factor(num.trees), y=accRatio)) + 
  geom_boxplot() +
  labs(title = 'accuracy ratios for randomly sampled sub-forests')

# as the number of trees in the sub-forest grows
# the mean accuracy ratio goes up and standard deviation goes down
plot(baseCase$stats[['mean accuracy ratio']]~baseCase$stats[['number of trees']]
     , main='randomly sampled subforest'
     , xlab='number of trees'
     , ylab='accuracy ratio'
     , type='b')
plot(baseCase$stats[['sd accuracy ratio']]~baseCase$stats[['number of trees']]
     , main='randomly sampled subforest'
     , xlab='number of trees'
     , ylab='standard deviation'
     , type='b')

baseCase$stats[['mean accuracy ratio']]

# save(baseCase , file='baseCase500trees.rda')
# load('baseCase500trees.rda')
