# 8. July 2021

# base case evaluation for the previously generated 10 data splits 
# with a new ranger random forest grown on each training set

# random sampling subforests in different sizes and 
# looking at their accuracy ratios (acc of sampled subforest / accuracy of full forest)

rm(list=ls())

library(dplyr)
library(ranger)
library(ggplot2)

# my own code
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

# load df (with factors) 
# load docN with N repetitions for 
## data splits and the ranger random forest build on the current train set
# load acc.oob - which we do not need for now
load('data/doc-500trees-10rep-5maxDepth-keepInbag.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
rm(acc.oob)
N <- length(docN)

# accuracy of the full forest on the validation set
aff<-c(rep(0,N))
par(mfrow=c(1,1))
for(i in 1:N) aff[i]<-docN[[i]]$accuracy[1]
boxplot(aff, 
        main=paste('accuracy full forest,',N,'repetitons\n(median:', median(aff) %>% round(3) , ', sd:', sd(aff) %>% round(3),')'))
summary(aff)
# focus on variation , measured by standard deviation
median(aff) ; mean(aff) ; sd(aff)

####################################
#### baseline : random sampling ####
####################################

size.sf<-c(3,5,7,11,13,17)
#size.sf<-c(10,20,30,40,50)

# set.seed(2020)
set.seed(1237)

rs<-data.frame()
for(i in 1:N){
  forest <- docN[[i]]$ranger$forest
  val<-docN[[i]]$val
  accff <- docN[[i]]$accuracy[['val']]
  new.row<-c(i,500,round(accff,5),1)
  rs<-rbind(rs,new.row)
  for(k in size.sf){
    # mean accuracy of randomly sampled sub-forests
    for(j in 1:100){
      acc.sf<- apsf(forest
                    , sample(1:forest$num.trees, k, replace = FALSE)
                    , df[val,])
      
      new.row<- c( i
                   , k
                   , acc.sf %>% round(5)
                   , ( acc.sf / accff ) %>% round(5)
      )
      rs<-rbind(rs,new.row)
    }    
  }
}
names(rs)<-c('forest','size','acc','acc.ratio')

##################################
#### evaluate and save as rda ####
##################################

stats<-rs %>% group_by(size) %>% summarise(mean(acc.ratio)
                                              , sd(acc.ratio)
                                              , mean(acc)
                                              , sd(acc)) %>% data.frame()
names(stats)<- c('size','mean.acc.ratio','sd.acc.ratio','mean.acc','sd.acc')

baseCase<- list(rs=rs , stats=stats)
#stats
#rm(rs,stats)

save(baseCase , file='data/baseCase-rs-01-500trees.rda')
#load('data/baseCase-rs-500trees.rda')

###################
#### visualize ####
###################

# accuracy ratios
ggplot(baseCase$rs, aes(x=as.factor(size), y=acc.ratio)) + 
  geom_boxplot() +
  labs(title = 'accuracy ratios for randomly sampled sub-forests')

# accuracies , not ratios
ggplot(baseCase$rs, aes(x=as.factor(size), y=acc)) + 
  geom_boxplot() +
  labs(title = 'accuracies of randomly sampled sub-forests and full forest\n(10 rep each)')

# as the number of trees in the sub-forest grows
# the mean accuracy ratio goes up and standard deviation goes down
plot(mean.acc.ratio~size
     , data = baseCase$stats[baseCase$stats$size<500,]
     , main='randomly sampled subforest'
     , xlab='size of sub-forest (=nr trees sampled)'
     , ylab='accuracy ratio'
     , type='b')
# numeric values just plotted
baseCase$stats$mean.acc.ratio # sizes in size.sf , and for the full forest
baseCase$stats$sd.acc.ratio

plot(sd.acc.ratio~size.sf 
     , data=baseCase$stats[baseCase$stats$size<500,]
     , main='randomly sampled subforest'
     , xlab='number of trees'
     , ylab='standard deviation'
     , type='b')

###############
#### model ####
###############

# don't do this:'
summary(lm(acc.ratio~0+size , data=baseCase$rs[baseCase$rs$size<500,]))
#plot(lm(acc.ratio~0+size , data=baseCase$rs[baseCase$rs$size<500,]))
# an acuracy ratio that goes through the origin is not a good model
# residuals vs fitted and normal Q-Q look bad

# this doesn't look good either
# but a model with an intercept close to 1 makes sense
lm1 <- lm(acc.ratio~size , data=baseCase$rs[baseCase$rs$size<500,])
summary(lm1)
# R sqrd almost 0, which is bad
plot(lm1)
# residuals and normal QQ look ok

# same for the accuracy (not acc ratio)
# intercept is close to the mean accuracy of the full forest 0.767, but smaller
lm2 <- lm(acc~size , data=baseCase$rs[baseCase$rs$size<500,])
summary(lm2)
# Intercept and size highly significant, sensible estimates
# Residual std error at 5% , ok
# R sqrd almost 0, not ok
#plot(lm2)

# we will try to get more information, mode predictors to improve the linear model.
# This will be in dataEval-baseCase-rs-02.R
