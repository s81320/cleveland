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

#autoplot(mb)
boxplot(mb, main='why the log on the y-axis?')
p<-print(mb)
# par(mfrow=c(1,1))
plot(x=1:6, y=p$mean, main='mean execution times when halving num.trees', ylab='time in ms', xlab='halving num.trees starting with full forest')

p$mean[3] / p$mean[1]
p$mean[4] / p$mean[1]

###########################################################################################
#### full forest and 10% sub-forest #######################################################
###########################################################################################

sf1<-subforest(docN[[1]]$ranger$forest,1:50)
sf2<-subforest(docN[[2]]$ranger, 1:50)
sf3<-subforest(docN[[3]]$ranger, 1:50)

mb <- microbenchmark(predict(docN[[1]]$ranger,data=df[val,1:10]) 
                     , predict(sf1,data=df[val,1:10])
                     , predict(docN[[2]]$ranger,data=df[val,1:10]) 
                     , predict(sf2,data=df[val,1:10])
                     , predict(docN[[3]]$ranger,data=df[val,1:10]) 
                     , predict(sf3,data=df[val,1:10])
)

#autoplot(mb)
boxplot(mb, main='full forest and 10% sub-forest')
p<-print(mb)
# par(mfrow=c(1,1))
plot(x=1:6, y=p$mean, main='full forest and 10% sub-forest', ylab='time in ms', xlab='alternating full forest and 10% sub-forest')

p$mean[2] / p$mean[1]
p$mean[4] / p$mean[3]
p$mean[6] / p$mean[5]

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
plot(x=1:6, y=p$mean, main='mean execution times for 50 trees', ylab='time in ms', xlab='samples of 50')

#######################################################################
#### forests of 1 and 2 trees, what is the minimum execution time? ####
#######################################################################


sf1<-subforest(rg$forest,1)
sf2<-subforest(rg$forest,2)
sf3<-subforest(rg$forest,3)
sf4<-subforest(rg$forest,4:5)
sf5<-subforest(rg$forest,6:7)
sf6<-subforest(rg$forest,8:9)

mb <- microbenchmark(t1a=predict(sf1,data=df[val,1:10])
                     , t1b=predict(sf2,data=df[val,1:10])
                     , t1c=predict(sf3,data=df[val,1:10])
                     , t2a=predict(sf4,data=df[val,1:10])
                     , t2b=predict(sf5,data=df[val,1:10])
                     , t2c=predict(sf6,data=df[val,1:10])
)

#autoplot(mb)
p<-print(mb)
# par(mfrow=c(1,1))
plot(x=1:6, y=p$mean, main='mean execution times for minimal forest\n(1 and 2 trees)', ylab='time in ms', xlab='minimal forest', xaxt='no')
axis(side = 1
     , at = 1:6
     , labels = c(1,1,1,2,2,2)) # rep(1:2,3) 

##################
#### all in 1 ####
##################


mb <- microbenchmark(ff=predict(rg,data=df[val,1:10])
                     , t250=predict(subforest(rg,1:250),data=df[val,1:10])
                     , t125=predict(subforest(rg,1:125),data=df[val,1:10])
                     , t62=predict(subforest(rg,1:62),data=df[val,1:10])
                     , t31=predict(subforest(rg,1:31),data=df[val,1:10])
                     , t16=predict(subforest(rg,1:16),data=df[val,1:10])
                     , t2=predict(subforest(rg,1:2),data=df[val,1:10])
                     , t1=predict(subforest(rg,1),data=df[val,1:10])
                     , times = 1000L
)

#autoplot(mb)
p<-print(mb)
# par(mfrow=c(1,1))
plot(x=c(500,250,125,62,31,16,2,1), y=p$mean, main='mean execution times', ylab='time in ms', xlab='number of trees', xaxt='no')
axis(side = 1
     , at = c(500,250,125,62,31,16,2,1)
     , labels = c(500,250,125,62,31,16,2,1))

boxplot(mb, log = T)

##################
#### all in 1 ####
##################


mb <- microbenchmark(predict(rg,data=df[val,1:10])
                     , predict(subforest(rg,1:450),data=df[val,1:10])
                     , predict(subforest(rg,1:400),data=df[val,1:10])
                     , predict(subforest(rg,1:350),data=df[val,1:10])
                     , predict(subforest(rg,1:300),data=df[val,1:10])
                     , predict(subforest(rg,1:250),data=df[val,1:10])
                     , predict(subforest(rg,1:200),data=df[val,1:10])
                     , predict(subforest(rg,1:150),data=df[val,1:10])
                     , predict(subforest(rg,1:100),data=df[val,1:10])
                     , predict(subforest(rg,1:50),data=df[val,1:10])
                     , predict(subforest(rg,1:30),data=df[val,1:10])
                     , predict(subforest(rg,1:10),data=df[val,1:10])
                     , predict(subforest(rg,1:1),data=df[val,1:10])
                     , times = 1000L
)

#autoplot(mb)
p<-print(mb)
# par(mfrow=c(1,1))
xgrid = c(seq(from=500, to=50 , length.out=10),30,10,1)
plot(x=xgrid
     , y=p$median
     , main='median execution times'
     , ylab='time in ms'
     , xlab='number of trees'
)
boxplot(mb, log = T, xaxt='n')
axis(1,at=xgrid,labels=xgrid)

