# http://adv-r.had.co.nz/Profiling.html

rm(list=ls())

# install.packages('microbenchmark')
library(microbenchmark)

library(dplyr)
library(ranger)
library(ggplot2)

# my own code
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/chipman.R')
source('code/source/helper-functions.R')

# load df (with factors) 
# load docN with N repetitions for 
## data splits and the ranger random forest build on the current train set
load('code/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

rg <- docN[[1]]$ranger
val<- docN[[1]]$val

# we can do this for different scenarios:
# Halving num.trees and calculating execution times for their prediction 

predict(rg,data=df[val,1:10])
sf1<-subforest(rg$forest,1:250)
sf2<-subforest(rg$forest,1:125)
sf3<-subforest(rg$forest,1:62)
sf4<-subforest(rg$forest,1:31)
sf5<-subforest(rg$forest,1:15)

mb <- microbenchmark(predict(rg,data=df[val,1:10]) 
               , predict(sf1,data=df[val,1:10])
               , predict(sf2,data=df[val,1:10])
               , predict(sf3,data=df[val,1:10])
               , predict(sf4,data=df[val,1:10])
               , predict(sf5,data=df[val,1:10])
               )

autoplot(mb)
p<-print(mb)
# par(mfrow=c(1,1))
plot(x=1:6, y=p$mean, main='mean execution times when halving num.trees', ylab='time in ms', xlab='halving num.trees')

p$mean[6] / p$mean[1]


##########################################
#### different scenario: with forests of 50 trees (10% of full forest) how much variation is there (depending on the trees?)
##########################################


sf1<-subforest(rg$forest,1:50)
sf2<-subforest(rg$forest,51:100)
sf3<-subforest(rg$forest,101:150)
sf4<-subforest(rg$forest,151:200)
sf5<-subforest(rg$forest,201:250)
sf6<-subforest(rg$forest,251:300)


mb <- microbenchmark(predict(sf1,data=df[val,1:10])
                     , predict(sf2,data=df[val,1:10])
                     , predict(sf3,data=df[val,1:10])
                     , predict(sf4,data=df[val,1:10])
                     , predict(sf5,data=df[val,1:10])
                     , predict(sf6,data=df[val,1:10])
)

#autoplot(mb)
p<-print(mb)
# par(mfrow=c(1,1))
plot(x=1:6, y=p$mean, main='mean execution times for 5 trees', ylab='time in ms', xlab='samples of 50')

max(p$mean) / min(p$mean)

mean(p$mean) / 6.77

