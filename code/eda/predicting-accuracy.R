rm(list=ls())

source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

library(dplyr)
library(ranger)

source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

# load basic data , like data set , data splits and forests
# should be the same as in dataEval-baseCase-rs-03.R to have a matching of full-forests and their sub-forests
# load list dataGen02 with info
load('data/10forests/500trees-10rep-5maxDepth-keepInbag.rda') 

dataGen02$info
N <- dataGen02$N
df<- dataGen02$df 
splitN <- dataGen02$splitN
rangerN <- dataGen02$rangerN
dmN <- dataGen02$dmN
acc.ff <- dataGen02$acc.ff
acc.oob <- dataGen02$acc.oob

# load tree indices of randomly sampled sub-forests and info on the sub-forests
file='data/10forests/baseCase-rs-sf.rda'
load(file)
baseCase.rs.sf$info %>% cat()
baseCase.rs.sf$content

# match the rs , size.sf with the ma.sf 
# and compare it to the performance of the underlying ranger (... to be loaded from else where ... who knows if they match?)
# on the validation and test sets.

# we have 3 kinds of accuracies : oob (on training data) , val , test
# this script is about validation and test accuracy of sub-forests and forests

#### problem statement ####
###########################
# one split , and its forest 
i<- 1
forest<- rangerN[[i]]$forest
val <-splitN$val[[i]]
test <- splitN$test[[i]]

# nRS random samples
rs<- baseCase.rs.sf$rs
nRS<-ifelse(all(lengths(rs)==length(rs[[1]])) , length(rs[[1]]), NULL)
nRS

# sub-forests of sandomly sampled trees
size.sf <- baseCase.rs.sf$size.sf
length(size.sf)

acc.sf <- baseCase.rs.sf$acc.sf[,,i]
dim(acc.sf)
assertthat::assert_that(length(size.sf)==nrow(acc.sf) , msg='1st index of acc.sf indicates the size of the sub-forest. Equality expected. Expectation missed.')

k<-4 # size 7 # fixed
a<-function(j){ # j in 1:nRS one of the random samples
  trindices <- rs[[i]][[j]][1:size.sf[k]]
  # length(acc.oob[[i]])

  return(c(mean(acc.oob[[i]][trindices])
           , acc.sf[k,j]
           , apsf(forest=forest , idx = trindices , newdata = df[test,])
           )
         )
}
b<-Vectorize(a)(1:nRS) # running over j , over all random samples
# dim(b) # 3 , 10
# columns of b : oob acc , val acc , test acc

plot(b[2,]-b[3,]
     , ylim=c(-0.15 ,0.15)
     , main=paste('problem: differences in acc often larger than 5%\n(subforests of size',size.sf[k],', split ', i,')')
     , ylab='difference in accuracy (val-test)'
     , xlab='independent repetition (nRS)')
abline(h=-0.05, col='grey')
abline(h=0.05, col='grey')
legend('bottomleft'
       , legend=c('diff in accuracy, sf','acceptable region?')
       , pch=c('o','-')
       , col=c('black','grey')
       , cex=0.8)

# hint : this may have something to do with 
# the difference in accuracy of the full forest on validation and test sets
p1<-function(b){
  plot(b[2,]-b[3,]
       , ylim=c(-0.15 ,0.15)
       , main=paste('problem: differences in acc often larger than 5%\n(subforests of size',size.sf[k],', split ', i,')')
       , ylab='difference in accuracy (val-test)'
       , xlab='independent repetition (nRS)')
  abline(h=-0.05, col='grey')
  abline(h=0.05, col='grey')
  abline(h=acc.ff$val.minus.test[[i]] , col='orange')
  legend('bottomleft'
       , legend=c('diff in accuracy, sub-forest','acceptable region?','diff in accuracy, full forest')
       , pch=c('o','-','-')
       , col=c('black','grey','orange')
       , cex=0.8)
}
p1(b)

#### problem : no way of predicting a good test accuracy ####
## neither from oob acc nor from val acc ####################
#############################################################


#### is the difference in accuracy for the full forest an indicator
#### for the difference in accuracies for the sub-forest built on this data split ??
#####################################################################################
#### the following is on the full forests and the data splits ####
##################################################################

toPlot <- dataGen02$acc.ff$val.minus.test

accept <- 0.05

good<- which(toPlot>-accept & toPlot<accept)
bad<- (1:N)[-good]

col <- rep(2,nRS)
col[good] <-3

plot(x=1:nRS
     , y= toPlot
     , type='p'
     , main='some forests perform very differently\non the validation and test set'
     , xlab='data splits , forest'
     , ylab="difference of val and test accuracy"
     , col = col)
abline(a=+accept,b=0, col='grey')
abline(a=-accept,b=0, col='grey')
legend('bottomleft'
       ,legend=c('accept','not accept')
       , col = unique(col)
       , pch='o'
       , cex =0.8)


# should we select data splits such that 
# accuracy of the full forest on the validation and test sets differs by at most 5%
# -> indicating that validation and test sets are not too different
# both accuracies lie within a corridor around the mean accuracy ?
# -> indicating that the training set is similarly distributed to both the validation and test set

# since these accuracies are calculated on the full forest, it is info about the underlying splits.
# when we work with the forests and subforests , we want to measure the effect of our selection.
# not the effect of the data splitting.

# NA to get vectors good and bad of the same size , to prevent recycling in cbind

#### looking into the sub-forests of good and bad data splits ####
##################################################################

A<-function(i){
  # a will now use A's argument i 
  # it is not the global i
  
  # # global variables used: rs, size.sf ,  val , test
  return(a)
}

k<-5
for(i in good){
  p1(Vectorize(A(i))(1:nRS))
}

for(i in bad){
  p1(Vectorize(A(i))(1:nRS))
}

# it does not matter , there is no difference b/w good and bad :-(
# for small sub forest sizes like 7 
# and still for subforests of 13 trees ...

# could I do a boxplot of distance??

#### no solution ####
#####################
