rm(list=ls())

library(dplyr)
library(ranger)

# source('code/source/distance-matrices-02.R') # my own code
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')
source('code/source/distance-matrices-03-scaled01.R')
source('code/source/chipman.R')
# load df (with factors) 
# load docN with N repetitions for 
## data splits and the ranger random forest build on the current train set
load('code/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

rg <- docN[[1]]$ranger
ranger::treeInfo(rg,1)
ranger::treeInfo(rg,2)
ranger::treeInfo(rg,3)

sf <- subforest(rg$forest, c(1,2,3))

d0<-function(a,b){length(base::union(a,b)) - length(base::intersect(a,b))}

nVar<-length(rg$forest$independent.variable.names)

# highly important in the full random forest are variables : 0, 7 , 9
luc<-mapply(function(a,b) unique(a[which(b[[1]]!=0)]) # get split vars for inner nodes
            , sf$split.varIDs
            , sf$child.nodeIDs
            , SIMPLIFY=F # returns a list, never a matrix
)

outer(luc,luc,Vectorize(d0))/nVar

D<-createDMd0(sf)
mds(D,xylim=T, mds='mds d0')

################### d1 ###################

predict(sf, # predict works with a ranger object or a ranger.forest object
        data=df[c(1,2,3),], # data.frame training
        predict.all=TRUE,
        type='terminalNodes'
)$predictions
# rows for observations , cols for trees , 
# entry (i,j) : nodeID the observation i is classified to in tree j

#D<-createDMd1(sf, dft=df[c(1,2,3,4,5,6),]) # rank 3
D<- createDMd1(sf, dft=df[c(1,2,3),]) # rank 2 , forming a line, not a plane
D
mds(D,TRUE, main='mds d1')

################### d2 ###################

predict(sf, # predict works with a ranger object or a ranger.forest object
        data=df[c(1,2,3),], # data.frame training
        predict.all = TRUE,
        # type='response' # it's the default
)$predictions

D<- createDMd2(sf, dft=df[c(1,2,3),]) # rank 2 , forming a line, not a plane
D
mds(D,TRUE, main='mds d2')

################### sb ###################

t1<-rt2fbt(rgf=sf, trindx=1)
t2<-rt2fbt(rgf=sf, trindx=2)
t3<-rt2fbt(rgf=sf, trindx=3)

A<- list(t1,t2,t3)
result<-outer(A,A,Vectorize(function(a,b)length(which(a!=b))))
result<-as.matrix(result)
diag(result)<- 0
result
D<- createDMsb(sf) 
D
mds(D,TRUE, main='mds sb')
