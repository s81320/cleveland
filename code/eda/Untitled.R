rm(list=ls())

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
#acc.ff <- dataGen02$acc.ff
acc.oob <- dataGen02$acc.oob

# load tree indices of randomly sampled sub-forests and info on the subforests
file='data/10forests/baseCase-rs-sf.rda'
load(file)
baseCase.rs.sf$info %>% cat()
baseCase.rs.sf$content

# match the rs , size.sf with the ma.sf 
# and compare it to the performance of the underlying ranger (... to be loaded from else where ... who knows if they match?)
# on the validation and test sets.

# we have 3 kinds of accuracies : oob (on training data) , val , test

# one forest 
i<- 1
forest<- rangerN[[i]]$forest
val <-splitN$val[[i]]
test <- splitN$test[[i]]

# nRS random samples
rs<- baseCase.rs.sf$rs
nRS<-ifelse(all(lengths(rs)==length(rs[[1]])) , length(rs[[1]]), NULL)
nRS

size.sf <- baseCase.rs.sf$size.sf
length(size.sf)

acc.sf <- baseCase.rs.sf$acc.sf[,,i]
dim(acc.sf)
assertthat::assert_that(length(size.sf)==nrow(acc.sf) , msg='1st index of acc.sf indicates the size of the sub-forest. Equality expected. Expectation missed.')

# good splits : 2 7 8 10
# bad splits 1 3 4 6 9 # 5 is somewhere in the middle
i<-1
k<-3
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
dim(b)
plot(b[1,] # b[1,] is oob acc
     , type='b'
     , ylim=c(min(b),max(b))
     , main=paste('predicting test accuricy?\n(subforests of size',size.sf[k],')')
     , ylab='accuracy'
     , xlab='independent repetition (nRS)')
points(b[2,], type='b', col=2) # b[2,] is validation accurcy
points(b[3,], type='b' , col=3) # b[3,] is test accuracy
legend('bottomright'
       , legend=c('mean oob acc', 'val acc', 'test acc')
       , col=1:3
       , pch='o'
       , cex=0.8)

a(1)

#### this is on the full forests ####
#####################################

plot(dataGen02$acc.ff$val.minus.test
     , type='b'
     , main='some forests perform very differently\non the validation and test set'
     , xlab='data split'
     , ylab="difference of val and test accuracy")
abline(a=0.05,b=0, col='grey')
abline(a=-0.05,b=0, col='grey')
# good splits : i in c(2,5,7,8,10)


plot(dataGen02$acc.ff$val
     , type='b'
     , ylim=c(min(c(dataGen02$acc.ff$val,dataGen02$acc.ff$test))
              , max(c(dataGen02$acc.ff$val,dataGen02$acc.ff$test)) )
     , main='fluctuation of accurcies for our data splits\n(full forest)'
     , xlab='data split'
     , ylab="accuracy")
points(dataGen02$acc.ff$test, type='b', col=2)
abline(a=0.74,b=0, col='grey')
abline(a=0.82,b=0, col='grey')
legend('bottomleft',legend=c('validation acc','test acc') , col=1:2 , pch='o',cex=0.8)
# same data input, same result:
# good splits : i in c(2,5,7,8,10)

# should we select data splits such that 
# accuracy of the full forest on the validation and test sets differs by at most 5%
# -> indicating that validation and test sets are not too different
# both accuracies lie within a corridor around the mean accuracy ?
# -> indicating that the training set is similarly distributed to both the validation and test set

# since these accuracies are calculated on the full forest, it is info about the underlying splits.
# when we work with the forests and subforests , we want to measure the effect of our selection.
# not the effect of the data splitting.

# Would this selection of splits give me better results ??
# reasonable splits : 2,(5),7,8,10

good <- c(2,5,7,8,10)
bad <- c(1,3,4,6,9)
# 5 is in between
# NA to get vectors good and bad of the same size , to prevent recycling in cbind

diff<-b[2,]-b[3,]

bp<-boxplot(cbind('good'=diff[good], 'bad'=diff[bad])
        , main='difference in val , test acc'
        , ann=T) 
bp
a(1)
A<-function(x){
  i<<-x
  return(a)}
A(good)(1)
B<-Vectorize(A(2))
b<-Vectorize(a)(1:nRS) # running over j , over all random samples

