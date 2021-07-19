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
load('data/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

rg <- docN[[1]]$ranger
val<- docN[[1]]$val

# predictions with a ranger sub-forest of size i on the validation set
predSF<-function(i){predict(subforest(rg,1:i),data=df[val,1:10])}

times<-100L # default
mb <- microbenchmark(predSF(50)
                     , predSF(100)
                     , predSF(150)
                     , predSF(200)
                     , predSF(250)
                     , predSF(300)
                     , predSF(350)
                     , predSF(400)
                     , predSF(450)
                     , predSF(500)
                     , times =times
)

#autoplot(mb)
p<-print(mb)
# par(mfrow=c(1,1))
xgrid<- seq(from=50, to=500 , by=50)

plot(x=xgrid
     , y=p$median
     , main=paste('median execution times\n(times=',times,')')
     , ylab=paste('time in',attr(p,'unit'))
     , xlab='number of trees'
)
lm1<-lm(median~as.numeric(expr), data=p)
abline(coefficients(lm1), col='grey')

# summary(lm1)
lm1$coefficients

boxplot(mb
        , log = T
        , main=paste('outliers in execution times for ranger predictions\n(times=',times,')' ) # many outliers!
)

#### repeting the above , small forests only ####

## with times = 100L the medians don't line up, too much variation still
## we use times = 1000L
times <- 1000L
mb <- microbenchmark(predSF(1)
                     , predSF(2)
                     , predSF(3)
                     , predSF(4)
                     , predSF(5)
                     , predSF(6)
                     , predSF(7)
                     , predSF(8)
                     , predSF(9)
                     , predSF(10)
                     , predSF(11)
                     , predSF(12)
                     , predSF(13)
                     , times=times
)

p <- print(mb)
xgrid <- c(1:13)
plot(x=xgrid
     , y=p$median
     , main=paste('median execution times\n(times=',times,')')
     , ylab=paste('time in',attr(p,'unit'))
     , xlab='number of trees'
)
lm1<-lm(median~as.numeric(expr), data=p)
abline(coefficients(lm1), col='grey')

# summary(lm1)
lm1$coefficients

boxplot(mb
        ,log = T
        , main=paste('outliers in execution times for ranger predictions\n(times=',times,')') # even more outliers!
)



# the following is not working
# this is the benchmark of the fingle function creates by Vecorize
#Vectorize(function(i){predict(subforest(rg,1:i),data=df[val,1:10])})(seq(0,500,50)) %>% 
#  microbenchmark(times=100L)

#### repeating code like this is not elegant ####
#### listing the arguments to the benchmark function (as above) is not elegant ####
###################################################################################

# what we have so far (which is not elegant)
f1<-function(i){predict(subforest(rg,1:i),data=df[val,1:10])}
microbenchmark(f1(1),f1(2))

# how to automate the sequence f(1),f(2) ?
p1<-paste('f1(',1:2,')',sep = '')
c1<-cat(p1,sep = ',')
# but this does not work as an argument to benchmark
microbenchmark(cat(p1,sep=','), times=10L)
do.call(what=p2[1], args=list(i=50)) # I do not want to CALL the function
c1
p1

# not working
# p2<-c(f1(1),f1(2)) # this is a vector of the function results! not what I need
# p2<-c('f1','f1') # this is a vector of characters but nobody knows they are meant to be functions...
microbenchmark(p2)
p2[1]
as.function() # not what I need
