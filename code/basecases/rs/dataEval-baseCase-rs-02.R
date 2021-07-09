rm(list=ls())

library(dplyr)
library(ranger)

source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

# load df (with factors) 
# load docN with N repetitions for 
## data splits and the ranger random forest build on the current train set
# load ibc (in bag count) , acc.oob (accuracy on oob training data for each tree) 
load('data/doc-500trees-10rep-5maxDepth-keepInbag.rda') 

N <- length(docN)

########################################################
#### baseline : random sampling ########################
#### new: collect info on tree distance and oob acc ####
########################################################

size.sf<-c(3,5,7,11,13)
#size.sf<-c(10,20,30,40,50)

nRS<-100 # number of random samples
rs<-data.frame(matrix(0,nrow=length(size.sf)*N*nRS+N,ncol=7)) 
# *nRS because we have nRS different random samples per full forest and per size.sf
# cf for(j in 1:nRS){ ...
# +N do also document the accuracies of the full forest, and we have N of them
dim(rs)
names(rs)<-c('forest','size','sampled.trees','acc', 'acc.ratio','mean.oob.acc','mean.d1.dissim')

ct<-1 # index counter

for(i in 1:N){
  forest <- docN[[i]]$ranger$forest
  val<-docN[[i]]$val
  accff <- docN[[i]]$accuracy[['val']]
  
  dm<-docN[[i]]$distMatrices$d1
  # preds<-predict(forest, data = df[val,], predict.all = TRUE) # don't need this ?? !!
  
  rs[ct,]<-data.frame(i
              , 500
              , '1:500'
              , round(accff,5)
              , 1
              , mean(acc.oob[i,]) %>% round(5) 
              , mean(dm[upper.tri(dm)])  %>% round(5)
              )
  ct<-ct+1

  for(k in size.sf){
    # mean accuracy of randomly sampled sub-forests
    for(j in 1:nRS){
      trindcs<-sample(1:forest$num.trees, k, replace = FALSE)
      accsf<-apsf(forest, trindcs, df[val,]) 

      rs[ct,]<- data.frame('forest'=i # which data split, which full forest
                  , 'size'=k # number of clusters, size of sub-forest
                  , 'sampled.trees'=paste(trindcs[order(trindcs)],collapse=',') # add this to check if something is strange
                  , 'acc'=accsf
                  , 'acc.ratio'=(accsf / accff) %>% round(5) # accuracy ratio on validation set
                  , 'mean.oob.acc'=mean(acc.oob[i,trindcs]) %>% round(5) # accuracy on oob training data
                  , 'mean.d1.dissim'=(sum(dm[trindcs,trindcs])/(k*(k-1)))  %>% round(5) # mean distance of sampled trees , not using mean
                  # , 'mean.d1.dissim'=mean(dm[trindcs,trindcs][upper.tri(dm[trindcs,trindcs])]  %>% round(5) # mean distance of sampled trees , not using mean
                  )
      ct<-ct+1
    }    
  }
}


##########################################
#### save documented / generated data ####
##########################################

stats<-rs %>% 
  group_by(size) %>% 
  summarise('mean.acc'=mean(acc)
            , 'sd.acc'=sd(acc)
            , 'mean.acc.ratio'=mean(acc.ratio)
            , 'sd.acc.ratio'=sd(acc.ratio)
            , 'mean.mean.oob.acc'= mean(mean.oob.acc)
            , 'sd.mean.oob.acc'=sd(mean.oob.acc)
            , 'mean.mean.d1.dissim'=mean(mean.d1.dissim)
            , 'sd.mean.d1.dissim'=sd(mean.d1.dissim)
            ) %>% 
  data.frame()

stats

#baseCase02<- list(info='sunny today 8-July-2021 :-)' , rs=rs , stats=stats)

#file='data/baseCase02-rs-500trees.rda'
#save(baseCase02 , file=file)
#load(file)
#rs<-baseCase02$rs
#stats<-baseCase02$stats

###################
#### visualize ####
###################

ggplot(rs, aes(x=as.factor(size), y=acc.ratio)) + 
  geom_boxplot() +
  labs(title = 'accuracy ratios for randomly sampled sub-forests')

ggplot(rs, aes(x=as.factor(size), y=acc)) + 
  geom_boxplot() +
  labs(title = 'accuracies for randomly sampled sub-forests')

# as the number of trees in the sub-forest grows
# the mean accuracy ratio goes up and standard deviation goes down
plot(mean.acc~size
     , data = stats[stats$size<500,]
     , main='randomly sampled subforest\n(accuracy)'
     , xlab='forest size'
     , ylab='mean accuracy'
     , type='b')
# numeric values just plotted
stats$mean.acc

plot(sd.acc~size
     , data = stats[stats$size<500,]
     , main='randomly sampled subforest\n(accuracy)'
     , xlab='forest size'
     , ylab='standard deviation for accuracy'
     , type='b')

plot(mean.acc.ratio~size
     , data = stats[stats$size<500,]
     , main='randomly sampled subforest\n(accuracy ratios)'
     , xlab='frest size'
     , ylab='mean accuracy ratio'
     , type='b')
# numeric values just plotted
stats$mean.acc.ratio

plot(sd.acc.ratio~size
     , data = stats[stats$size<500,]
     , main='randomly sampled subforest\n(accuracy ratios)'
     , xlab='forest size'
     , ylab='standard deviation for accuracy ratio'
     , type='b')

##########################################
#### linear models for interpretation ####
##########################################

lm1<-lm(acc.ratio~0+size+mean.oob.acc+mean.d1.dissim , data=rs[rs$size<500,])
summary(lm1)
# all predictors highly significant, R-sqrd almost 1 -> all variance explained
# this is good!!

# now with an intercept / a constant
lm2<-lm(acc.ratio~size+mean.oob.acc+mean.d1.dissim , data=rs[rs$size<500,])
summary(lm2)
lm3<-lm(acc.ratio~mean.d1.dissim , data=rs[rs$size<500,])
summary(lm3)
# this is so poor :-(
# R sqrd almost 0 , mean oob acc and mean d1 dissim not significant

#plot(lm1)

# this is what we want in both models: lm1 and lm2
predict(lm1,
        newdata=data.frame('size'=11 , 'mean.oob.acc'=0.7 , 'mean.d1.dissim'=0.3))

# why is this so bad ??
plot(acc.ratio~mean.oob.acc 
     # , data=rs[(rs$size.sf==13 & rs$mean.d1.dissim>0.3),]
     , data=rs[rs$size<500,]
     , asp=1 
     , xlab='mean individual oob accuracy'
     , ylab='ensemble accuracy ratio (on val set)'
     , main='explain forest performance\nby mean of tree performance')

# because the res std error is so large? 6.7% can make a huge difference : 0.94 or 1.06 accuracy ratio?

library(ggplot2)
#install.packages('ggExtra')
library(ggExtra) # for the marginal plot
#install.packages('hrbrthemes')
library(hrbrthemes) # theme_ipsum

p1 <- ggplot(rs[rs$size<500,], aes(x=mean.oob.acc, y=acc.ratio, colour=size)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="blue", se=TRUE) +
  theme(legend.position="bottom")
p2 <- ggMarginal(p1, type="density")
p2
rm(p1,p2)

# maybe this is not so bad?
# there is a clear upward slope with a tight conf region around it
# increasing nRS makes it really tight
# we are dealing with small change here, an increased accuracy ratio of 3% is quite a lot

# -> that is why the residual std error of around 6% bothers me ... ups, negative vibes again

#######################
#### dissimilarity ####
#######################

p3 <- ggplot(rs[rs$size<500,], aes(x=mean.d1.dissim, y=acc.ratio, colour=size)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="blue", se=TRUE) +
  theme(legend.position="bottom")
 
p4 <- ggMarginal(p3, type="density")

p4
rm(p3,p4)

# the line is less steep , as expected from the linear model BUT
# for rather dissimilar trees (high values of mean.d1.dissim) most acc.ratios are high. There is only one exception
# as would be expected, small forests have a higher spread over the dissimilarity values
# repeating this (different sampling) it does not hold: rather dissimilar trees most acc.ratios are high!

####################################################
#### remains from developing the myAcc function ####
####################################################
#### will soon go ... ##############################
####################################################

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
