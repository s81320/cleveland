# http://adv-r.had.co.nz/Profiling.html

rm(list=ls())

# install.packages('microbenchmark')
library(microbenchmark)

library(dplyr)
library(ranger)

# my own code
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)

# load df (with factors) 
# load docN with N repetitions for 
## data splits and the ranger random forest build on the current train set
load('code/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

rg <- docN[[1]]$ranger
val<- docN[[1]]$val

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
boxplot(mb, log = T) # many outliers!