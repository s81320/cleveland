# input: cleveland data

# output: a list of N times
## a data split of the observations (indices) into training, validation and test
## a ranger random forest grown on the training data
## distance matrices for the trees in the forest

# start own environment?
setwd("~/Documents/ds/sem4/thesis/trees-02-ranger/04-chipman")
library(ranger)
library(caret)
library(e1071)
library(cluster)
library(dplyr)

source('distance-matrices-02.R') # my own code
source('subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('chipman-plots.R')
source('helper-functions.R')

df <- read.csv('../cleveland/Cleve.data.csv')[-1] # not elegant, but csv reads an empty first column as X

if(all(dim(df)==c(303,11))){paste
  }else{message(paste('did read an unexpected dimension of',dim(df),'for the cleveland dataset.')) }

if(length(is.na(df))>0){message('there seems to be data missing in cleveland dataset.')}

df$Sex<-as.factor(df$Sex)
df$Chestpaintype<-as.factor(df$Chestpaintype)
df$HighFastBloodSugar<-as.factor(df$HighFastBloodSugar)
df$RestingECG<-as.factor(df$RestingECG)
df$ExInducedAngina<-as.factor(df$ExInducedAngina)
df$CAD<-as.factor(df$CAD)

docN<-list()
doc<-list()

N=10
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
               , num.trees = 4
               #, num.trees=1000
               , importance='impurity'
               #, mtry = 3
               , max.depth=3
  )
  
  accVal<-apsf(rg$forest, 1:rg$num.trees , df[val,])
  accTest<-apsf(rg$forest, 1:rg$num.trees , df[test,])
  
  for(metric in c('d0','d1','d2','sb')){
    #for(metric in c('d0')){
    print(paste('start ' , metric , ' at ', Sys.time()) )
    dm2<-createDM(forest=rg$forest, type=metric , dft=df[train,])
    doc[[metric]]<-dm2
  }
  
  docN[[i]]<-list(train=train,val=val,test=test,ranger=rg,accuracy=c('val'=accVal,'test'=accTest),distMatrices=doc)
}

file=paste('doc-',rg$num.trees,'trees.rda',sep='')
save(df,train,val,test,rg,doc, file=file)
