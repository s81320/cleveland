# input: cleveland data

# output: a list of N times
## a data split of the observations (indices) into training, validation and test
## a ranger random forest grown on the training data
## distance matrices for the trees in the forest, based on 4 different metrices

rm(list=ls())
library(ranger)
library(caret)
library(e1071)
library(cluster)
library(dplyr)

# my own code
source('code/source/distance-matrices-03-scaled01.R') 
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

# read cleveland data
df <- read.csv('data//Cleve.data.csv')[-1] # not elegant, but csv reads an empty first column as X

if(all(dim(df)==c(303,11))){ 
  print('checked dimensions of loaded cleveland dataset: (303,11) : OK')
  }else{message(paste('did read an unexpected dimension of',dim(df),'for the cleveland dataset. Expected (303,11).')) }

if(length(which(is.na(df))>0)){message('there seems to be data missing in cleveland dataset.')}

df$Sex<-as.factor(df$Sex)
df$Chestpaintype<-as.factor(df$Chestpaintype)
df$HighFastBloodSugar<-as.factor(df$HighFastBloodSugar)
df$RestingECG<-as.factor(df$RestingECG)
df$ExInducedAngina<-as.factor(df$ExInducedAngina)
df$CAD<-as.factor(df$CAD)

# split data repeatedly (N times) into training, validation and test sets (w/ statification wrt target)
# train ranger on each training set
# document accuracy of full forest predictions on validation and test set
# create and document distance matrices for 4 metrices

N<-10
docN<-list(rep(NA,N))
doc<-list(rep(NA,4))


set.seed(1789) 
trainN <- createDataPartition(df$CAD
                            , list=TRUE
                            , p=0.7
                            , times=N)

for(i in 1:N){

  train <- trainN[[i]]

  # all rows except for training
  test <- setdiff(1:nrow(df),train)

  # half of the test set , stratified
  val <- test[createDataPartition(df[test,]$CAD, list=TRUE)[[1]]] # times=1 , p=0.5 are  defaults

  # remaining half of the test set
  test<-setdiff(test,val)

  set.seed(i) 
  rg <- ranger(CAD~.
               , data = df[train,]
               , num.trees = 500
               #, num.trees=1000
               , importance='impurity'
               #, mtry = 3
               , max.depth=5
               , min.node.size=5
               , keep.inbag = T
  )
  
  accVal<-apsf(rg$forest, 1:rg$num.trees , df[val,])
  accTest<-apsf(rg$forest, 1:rg$num.trees , df[test,])
  
  for(metric in c('d0','d1','d2','sb')){
    #for(metric in c('d0')){
    print(paste('loop', i, 'start' , metric , 'at', Sys.time()) )
    dm2<-createDM(forest=rg$forest, type=metric , dft=df[train,])
    doc[[metric]]<-dm2
  }
  
  docN[[i]]<-list(train=train
                  , val=val
                  , test=test
                  , ranger=rg
                  , accuracy=c('val'=accVal,'test'=accTest)
                  , distMatrices=doc
                  )
}

# different approach , rather sequential. not looping so much:
# 1) create N data splits
# 2) grow a ranger random forest on each train set
# 3) calculate distance matrices on each forest

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


file=paste('code/doc-',rg$num.trees,'trees-',N,'rep-',rg$call$max.depth,'maxDepth-keepInbagTESTRUN.rda',sep='')
save(df,docN, ibc, acc.oob , file=file)
message(paste('saved data frame of cleveland data and \ndocumentation of generated splits, forests, forest accuracies, distance matrices, \ninbag counts and oob accuracies for each tree in\n' 
              , file 
              , '\nplease remove TESTRUN from the file name ... if this was not just a test run'))
