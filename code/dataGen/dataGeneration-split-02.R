# input: cleveland data

# output / result:
# generate and save as a R data file to be used for development of an algorithm to find small subforest with high accuracy ratios

# data to be generated and saved:
# df : the cleveland data frame
# splitN : N repetitions of splitting the observations in the cleveland data into training, validation and test sets
# rangerN : N ranger objects , each built from a separate training split generated in splitN
# dmN : 4*N distance matrices, 4 for each ranger forest from rangerN
# accuracies for the full forests (on validation and training sets)
# acc.oob : out of bag accuracies for each tree in each forest 

rm(list=ls())
library(ranger)
library(caret)
#library(cluster)
library(dplyr)


# my own code
source('code/source/distance-matrices-03-scaled01.R') 
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

# read cleveland data
df <- read.csv('data/Cleve.data.csv')[-1] # not elegant, but csv reads an empty first column as X

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

#### 1. create splits ####
##########################

set.seed(1789) 
trainN <- createDataPartition(df$CAD
                            , list=T
                            , p=0.7
                            , times=N)

splitN<-list(train=list(),val=list(),test=list())

for(i in 1:N){
  
  # initial data required (function argument) 
  #######################
  train <- trainN[[i]]
  
  # work / create (function body)
  ################
  # all rows except for training
  test <- setdiff(1:nrow(df),train)
  
  # half of the test set , stratified
  val <- test[createDataPartition(df[test,]$CAD, list=TRUE)[[1]]] # times=1 , p=0.5 are  defaults
 
  # remaining half of the test set
  test<-setdiff(test,val)
  
  # document (function return value)
  ##########
  splitN$train[[i]] <- trainN[[i]] # to document
  splitN$val[[i]]<- val
  splitN$test[[i]]<- test
  
}


#### 2. grow forests on training data ####
##########################################

rangerN <- list(rep(0,N))

for(i in 1:N){
  train<-splitN$train[[i]]
  
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
  rangerN[[i]]<-rg
}

#### 3. calculate distance matrices ####
########################################

metrices <- c('d0','d1','d2','sb')

dmN <- list()
for(m in metrices){
  dmN[[m]]<-list()
}


for(i in 1:N){
  forest<-rangerN[[i]]$forest
  train<- splitN$train[[i]]
  
  # print(paste('loop', i, 'start at', Sys.time()) )
  
  for(metric in metrices){
    
    dmN[[metric]][[i]]<-createDM(forest=forest, type=metric , dft=df[train,])
}
}

#### 4. calculate oob accuracies ####
#####################################

myAcc <-function(tri,i) {
  
  train <- splitN$train[[i]]
  sf <- subforest(rangerN[[i]], tri)
  
  inbag.counts <- rangerN[[i]]$inbag.counts[[tri]]
  oob<-which(inbag.counts==0)

  predict(
    object = sf
    , data = df[train[oob],]
  ) %>%
    .$predictions %>%
    as.numeric ->
    preds
  
  return(acc(preds
             ,as.numeric(df[train[oob],'CAD'] ))
  )
}

# check
# myAcc(1,1)

acc.oob.matrix<-outer(X=1:500 , Y=1:N, FUN=Vectorize(myAcc)) # X for the repetitions, Y for the trees
# res[1:2,1:3] # why the Numeric??
# dim(acc.oob) # check : N , rg$num.trees

acc.oob<- list()
for(i in 1:N){
  acc.oob[[i]]<-acc.oob.matrix[,i]
}

acc.ff<-list(val=rep(0,N), test=rep(0,N),val.minus.test=rep(0,N))

for(i in 1:N){
  val<- splitN$val[[i]]
  test<-splitN$test[[i]]
  acc.ff$val[i]<- acc( predict(rangerN[[i]]$forest
                                , df[val,])$predictions,df[val,'CAD'])
  acc.ff$test[i]<-acc(predict(rangerN[[i]]$forest, df[test,])$predictions,df[test,'CAD'])
}
acc.ff$val.minus.test <- acc.ff$val - acc.ff$test


#### save rda object ####
#########################

dataGen02<-list('info'=paste('created with dataGeneration-split-02.R at', Sys.time() ) 
     , 'N'=N
     , 'df'=df
     , 'splitN'= splitN
     , 'rangerN'=rangerN 
     , 'dmN'= dmN 
     , 'acc.ff'=acc.ff
     , 'acc.oob'=acc.oob)

file=paste('data/10forests/',rangerN[[1]]$num.trees,'trees-',N,'rep-',rangerN[[1]]$call$max.depth,'maxDepth-keepInbagTESTRUN.rda',sep='')
save(dataGen02 , file=file)
message(paste('saved data frame of cleveland data and \ndocumentation of generated splits, forests, forest accuracies, distance matrices, \n oob accuracies for each tree in\n' 
              , file 
              , '\nplease remove TESTRUN from the file name ... if this was not just a test run'))
