# different coding style 
# more sequential, less looping
# doing all sampling as one step, then proceed to next step. 
# Instead of generating one sample, working with it and then generating the next sample

# new: visualize the random selection in tree space

rm(list=ls())

library(dplyr)
library(ranger)
library(caret)

source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

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

########################################################
#### baseline : random sampling ########################
#### new: collect info on tree distance and oob acc ####
########################################################

#size.sf<-c(3,5)
size.sf<-c(3,5,7,11,13,17)
#size.sf<-c(10,20,30,40,50)

nRS<-100 # number of random samples per full forest 
# will generate nRS samples in each size for the sub-forest as specified in size.sf

rs<- list()
p<-0.1

assertthat::assert_that(p*rangerN[[1]]$num.trees >= max(size.sf))

set.seed(1237)
for(i in 1:N){
rs[[i]]<- createDataPartition(y=1:500 , p=p , times =nRS )
}
rs[[1]]

# samples for different sizes will be from the same partition
# directly ...
s<-2
rs[[1]][[1]][1:size.sf[s]]
# ... or vectorised
a<-Vectorize(function(s)rs[[1]][[1]][1:s])
a(size.sf)
# results in nested samples

#### for each subforest calculate
# accuracies on validation set , mean oob accuracy , mean distance

acc.sf <- array(0, dim=c(length(size.sf), nRS , N))
moa.sf <- array(0, dim=c(length(size.sf), nRS , N))
md.sf  <- array(0, dim=c(length(size.sf), nRS , N))

for(i in 1:N){
  forest <- rangerN[[i]]$forest
  val <- splitN$val[[i]]
  dm <- dmN$d0[[i]]
  ao <- acc.oob[[i]]
  
  for(j in 1:nRS){
    trindcs.all<-rs[[i]][[j]] # this is a vector, we only need the first size.sf[k] entries
    
    for(k in 1:length(size.sf)){
      trindcs<-trindcs.all[1:size.sf[k]]
      
      apsf(forest, trindcs, df[val,]) %>% 
        round(5) -> 
        acc.sf[k,j,i]
      
      mean(ao[trindcs]) %>% 
        round(5)->
        moa.sf[k,j,i]
      
      assertthat::assert_that(size.sf[k]==length(trindcs), msg = 'size of sub-forest and length of tree indices do not match')
      (sum(dm[trindcs,trindcs])/(size.sf[k]*(size.sf[k]-1)))  %>% 
        round(5) ->
        md.sf[k,j,i]
      
    }
  }
}

#### make it a tidy data table ####

td <- data.frame(list(forest=1,'acc.ff'=0.5,'size'=3,'acc.sf'=0.5,'moa.sf'=0.5,'md.sf'=0.5))
#nrow(td)<-N*nRS*length(size.sf)

ct<-1
for(i in 1:N){
  for(j in 1:nRS){
    for(k in 1:length(size.sf)){
        td[ct,]<- c(i
                    , acc.ff$val[i]
                    , size.sf[k]
                    , acc.sf[k,j,i]
                    , moa.sf[k,j,i]
                    , md.sf[k,j,i])
        ct<-ct+1
        }
    }
}

# add column for acc ratio
td$acc.ratio <- td$acc.sf / td$acc.ff

##########################################
#### save documented / generated data ####
##########################################

stats<-td %>% 
  group_by(size) %>% 
  summarise('mean.acc'=mean(acc.sf)
            , 'sd.acc'=sd(acc.sf)
            , 'mean.acc.ratio'=mean(acc.ratio)
            , 'sd.acc.ratio'=sd(acc.ratio)
            , 'mean.mean.oob.acc'= mean(moa.sf)
            , 'sd.mean.oob.acc'=sd(moa.sf)
            , 'mean.mean.d1.dissim'=mean(md.sf)
            , 'sd.mean.d1.dissim'=sd(md.sf)
            ) %>% 
  data.frame()

stats

# why is the mean of the mean oob accuracy so high, 81% ? The full forest has much less, 77%

#baseCase02<- list(info='sunny today 8-July-2021 :-)' , rs=rs , stats=stats)

#file='data/baseCase02-rs-500trees.rda'
#save(baseCase02 , file=file)
#load(file)
#rs<-baseCase02$rs
#stats<-baseCase02$stats

###################
#### visualize ####
###################

ggplot(td, aes(x=as.factor(size), y=acc.ratio)) + 
  geom_boxplot() +
  labs(title = 'accuracy ratios for randomly sampled sub-forests')

ggplot(td, aes(x=as.factor(size), y=acc.sf)) + 
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

summary(lm(acc.ratio~. , data= td)) # ??

# summary(lm(acc.ratio~. , data= td[,c(3,5:7)]))

lm1<-lm(acc.ratio~size+moa.sf+md.sf , data=td)
summary(lm1)
# this is so poor :-(
# R sqrd almost 0 , mean oob acc and mean d1 dissim not significant
# and with negative estimates :-(


plot(lm1)

# don't need this for now ...
predict(lm1,
        newdata=data.frame('size'=11 , 'moa.sf'=0.7 , 'md.sf'=0.3))

# why is this so bad ??
plot(acc.ratio~moa.sf 
     # , data=rs[(rs$size.sf==13 & rs$mean.d1.dissim>0.3),]
     , data=td
     , asp=1 
     , xlab='mean individual oob accuracy'
     , ylab='ensemble accuracy ratio (on val set)'
     , main='explain forest performance\nby mean of tree performance')

library(ggplot2)
#install.packages('ggExtra')
library(ggExtra) # for the marginal plot
#install.packages('hrbrthemes')
library(hrbrthemes) # theme_ipsum

p1 <- ggplot(td, aes(x=moa.sf, y=acc.ratio, colour=size)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="blue", se=TRUE) +
  theme(legend.position="bottom")
p2 <- ggMarginal(p1, type="density")
p2
rm(p1,p2)

#######################
#### dissimilarity ####
#######################

p3 <- ggplot(td, aes(x=md.sf, y=acc.ratio, colour=size)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="blue", se=TRUE) +
  theme(legend.position="bottom")
 
p4 <- ggMarginal(p3, type="density")

p4
rm(p3,p4)
