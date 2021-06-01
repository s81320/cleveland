# we have a train, validation , and test set
# we have a ranger random forest built on the training set.
# make predictions for the validation data and measure accuracy.
# do the same for the test data.
# calculate the correlation between the predictions

rm(list=ls())
setwd("~/Documents/ds/sem4/thesis/trees-02-ranger/03")

library(ranger)
library(caret)
library(e1071)
library(cluster)
library(dplyr)
#source('distance-matrices-02.R') # my own code
source('subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('helper-functions.R')

# read and transform the data

df <- read.csv('Cleve.data.csv')[-1] # not elegant, but csv reads an empty first column as X

df$Sex<-as.factor(df$Sex)
df$Chestpaintype<-as.factor(df$Chestpaintype)
df$HighFastBloodSugar<-as.factor(df$HighFastBloodSugar)
df$RestingECG<-as.factor(df$RestingECG)
df$ExInducedAngina<-as.factor(df$ExInducedAngina)
df$CAD<-as.factor(df$CAD)

dim(df) # 303 , 11 with 10 predictor variables
head(df)
table(is.na(df)) # quick check for missing data

set.seed(1685) # quite different accuracy on validation and test set
#set.seed(1955) # similar accuracy on validation and test set
N<-1
train.N<-caret::createDataPartition(df$CAD, p = .7, 
                                    list = FALSE, 
                                    times = N)

train<- train.N[,1]

# all rows except for training
test <- setdiff(1:nrow(df),train)

# half of the test set , stratified
val <- test[createDataPartition(df[test,]$CAD, list=TRUE)[[1]]] # times=1 , p=0.5 are  defaults

# remaining half of the test set
test<-setdiff(test,val)

checkPartition(train,val,test, df)

rg <- ranger(CAD~.
             , data = df[train,]
             #, num.trees = 23
             , num.trees=500
             , importance='impurity'
             #, mtry = 3
)

# compare to accuracy on test and validation set
predict(rg$forest, data=df[test,])$predictions %>% # pff : p_redict f_ull f_orest
  acc(.,df[test,'CAD'])
predict(rg$forest, data=df[val,])$predictions %>% # pff : p_redict f_ull f_orest
  acc(.,df[val,'CAD'])
# This is a huge difference in accuracy, this renders the performance on the validation set worthless.
# Is this bad luck? Is it the 'normal' behaviour?

# We look at the situation in more detail:
# (val-)and test-) accuracies for all trees in the forest
a1<-predict(rg$forest,
            df[val,],
            predict.all=TRUE)$predictions %>% # when using predict.all predictions are numeric
  apply(.,
        2, 
        function(vec) acc(vec,as.numeric(df[val,'CAD'])) # so we have to make the target numeric, too
)


a2<-predict(rg$forest,
            df[test,],
            predict.all=TRUE)$predictions %>% 
  # when using predict.all predictions are numeric
  apply(.,
        2, 
        function(vec) acc(vec,as.numeric(df[test,'CAD'])) # so we have to make the target numeric, too
  )

plot(jitter(a1)
     , jitter(a2)
     , xlab='accuracy on validation set'
     , ylab='accuracy on test set'
     , main='fixed forest, comparing for each tree accuracy\non validation and test set'
     )
# this plot looks not very strongly correlated, pretty cloudy, not like a line at all!
# which agrees with the small covariance

lm.obj<-lm(a2~0+a1) # explain test accuracy by validation accuracy
summary(lm.obj)
# the estimated slope is close to 1 with a really small p-value (good for us, highly significant)
# R-squared (explained variance) is small, also good for us.

# The residual standard error at 8.5% is what has bothered me before: it is too big for what I need.

# linear models should be applied when x values are deterministic. This is not the case here!
# how much does that matter??

plot(lm.obj)
# interpretation:
# Residuals vs fitted: clearly not random, with a strong tendency to decrease (red line) : regression to the mean, OK:
# if a tree performs above average on the validation set then it is likely to perform worde on the test set (because it cannot get much better)
# if a tree performs poorly on the validation set, it is likely to perform better on the test set (because it cannot get much worse)

# Q-Q plot: pretty much on the diagonal, very nice

# scale-location : ??

# residuals vs leverage : pretty small leverage! Well, in a forest of 500, how much influence can a single tree have?!

#treeInfo(rg,106) # this tree has a lot of influence?! In a good way, improving accuracy?
#apsf(rg$forest, 106 , df[val,]) ; apsf(rg$forest, 106 , df[test,]) # an especially big difference in accuracy
# ok performance on the validation set but really poor on test

#treeInfo(rg,90) # this tree has a lot of influence?! In a good way, improving accuracy?
#apsf(rg$forest, 90 , df[val,]) ; apsf(rg$forest, 90 , df[test,]) # an especially big difference in accuracy
# poor on validation set, great on test set

# check how the slope is calculated
sum(a1*a2) / sum(a1*a1) ; lm.obj$coefficients
# covariance
cov(a1,a2) ; sum((a1-mean(a1))*(a2-mean(a2)))/(length(a1)-1) # slightly different from mean((a1-mean(a1))*(a2-mean(a2)))
# correlation
stats::cor(a1,a2) ; cov(a1,a2) / (sd(a1)*sd(a2)) # in [-1,1] ,  pretty zero'ish... hmpf.

##############################################################
## we have 2 important influencers for the accuracy: 
# 1) the data partition into val and test set
# 2) the forest built on the train set (which relies on the data partition into training and non-training)
##############################################################

# look into 1)
# keep the forest fixed and vary the data partitions of validation and test set.
##############################################################

#set.seed(1685)
set.seed(1955)

N<-1
train.N<-caret::createDataPartition(df$CAD, p = .7, 
                                    list = FALSE, 
                                    times = N)
train<- train.N[,1]

rg <- ranger(CAD~.
             , data = df[train,]
             #, num.trees = 23
             , num.trees=500
             , importance='impurity'
             #, mtry = 3
)

doc3 <- data.frame()

N<-10
for(i in 1:N){
  
  # all rows except for training
  test <- setdiff(1:nrow(df),train)

  # half of the test set , stratified , based on a set 1,2,3,... , length(test) , returning indices if test-indices (not original df-related indices)
  split <- caret::createDataPartition(df[test,]$CAD, p = .5, 
                                    list = FALSE, 
                                    times = N)
  val <- split[,i] %>% test[.]
  #  val <- test[createDataPartition(df[test,]$CAD, list=TRUE)[[1]]] # times=1 , p=0.5 are  defaults

  # remaining half of the test set
  test<-setdiff(test,val)

  checkPartition(train,val,test, df)
  
  # accuracies for predictions of full forest on validation and training set
  av<-predict(rg$forest,df[val,])$predictions %>% acc(.,df[val,'CAD'])
  at<-predict(rg$forest,df[test,])$predictions %>% acc(.,df[test,'CAD'])

  new.row<-c(av,at)
  doc3<-rbind(doc3,new.row)
}

xgrid<-1:nrow(doc3)

plot(x=xgrid 
     , y=doc3[,1]
     , main='accuracy on validation and test set\n(same forest, different data partitions into val and test)'
     , xlim=c(1,nrow(doc3))
     , ylim=c(min(doc3[c(1,2)]) , max(doc3[c(1,2)]))
     , xlab='data partition'
     , ylab='accuracy'
     )

points(x=xgrid , y=doc3[,2] , col='blue')
legend('topright', legend=c('val','test') , col=c('black','blue'), pch='o')

# there is an obvious symmetry here and it tells me that the experiment setup makes no sense: As soon as the non-training data is fixed 
# and separated into test and validation data the resulting accuracies depend on how the non-training data is distributed into the
# val vs test bins. You can be lucky or not and it doesn't tell you anything about the learned model ...

##############################################################

#set.seed(1685)
set.seed(1955)

doc3 <- data.frame()

N<-100
train.N<-caret::createDataPartition(df$CAD, p = .7, 
                                    list = FALSE, 
                                    times = N)
for(i in 1:N){
  
  train<- train.N[,i]
  
  rg <- ranger(CAD~.
               , data = df[train,]
               #, num.trees = 23
               , num.trees=500
               , importance='impurity'
               #, mtry = 3
  )
  
  # all rows except for training
  test <- setdiff(1:nrow(df),train)
  
  # half of the test set , stratified , based on a set 1,2,3,... , length(test) , returning indices if test-indices (not original df-related indices)
  val <- test[createDataPartition(df[test,]$CAD, list=TRUE)[[1]]] # times=1 , p=0.5 are  defaults
  
  # remaining half of the test set
  test<-setdiff(test,val)
  
  checkPartition(train,val,test, df)
  
  # accuracies for predictions of full forest on validation and training set
  av<-predict(rg$forest,df[val,])$predictions %>% acc(.,df[val,'CAD'])
  at<-predict(rg$forest,df[test,])$predictions %>% acc(.,df[test,'CAD'])
  
  new.row<-c(av,at)
  doc3<-rbind(doc3,new.row)
}

xgrid<-1:nrow(doc3)

plot(x=xgrid 
     , y=doc3[,1]
     , main='accuracy on validation and test set\n(different data partitions into train, val and test)'
     , xlim=c(1,nrow(doc3))
     , ylim=c(min(doc3[c(1,2)]) , max(doc3[c(1,2)]))
     , xlab='data partition'
     , ylab='accuracy'
)

points(x=xgrid , y=doc3[,2] , col='blue')
legend('topright', legend=c('val','test') , col=c('black','blue'), pch='o')

# H0 : the forests accuracy on val and test set is the same
t.test(doc3[,1],doc3[,2]) # cannot reject H0 on a 5% level (on any level, pvalue>95%)
# What does this mean for a fixed validation and test set and a fixed forest?
# a) the difference in accuracy on validation and test data can be expected to be 10% (mean +- 1 sd)
mean(abs(doc3[,1]-doc3[,2])) # L1
sd(abs(doc3[,1]-doc3[,2])) # L1
# b) half the time the difference will be 4.7% (median) (or 6.5% (the mean)?) and larger
# in 25% of the cases the difference will be 7.9% (3rd quantil) and larger
summary(abs(doc3[,1]-doc3[,2]))
# This is not good enough, I think.
# When picking high performers on the validation set
# the distribution of error differences (b/w error on val set and error on test set) may be different / more extreme!

names(doc3)<-c('acc.val','acc.test')

library("ggpubr")
ggscatter(doc3, x = "acc.val", y = "acc.test", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "accuracy on vlidation set", ylab = "accuracy on test set")

#doc3<- unique(doc3)
cor.test(doc3[,1],doc3[,2], method = 'spearman')

plot(x=jitter(doc3[,1])
     , y=jitter(doc3[,2])
     , xlim=c(0.5,1)
     , ylim=c(0.5,1)
     , xlab='val accuracy'
     , ylab='test accuracy'
     , main='correlation of val and test accuracy\n(different data partitions into train, val, and test set)')
abline(a=0, b=1, col='grey') # what I would like to see
doc3.lm.1<-lm(acc.test~0+acc.val , data=doc3) # 1 parameter regression line (only slope)
abline(doc3.lm.1, lty=32)
doc3.lm.2<-lm(acc.test~acc.val , data=doc3) # 2 parameter regression line (intercept and slope)
abline(doc3.lm.2, lty=3)

summary(doc3.lm.1) # residual Std error pretty much the same as for accuracy of trees :  lm.obj
###################################################################
## keep data partition fixed and build a new forest in each loop ##
###################################################################

set.seed(1685)
#set.seed(1955)
N<-1
train.N<-caret::createDataPartition(df$CAD, p = .7, 
                                    list = FALSE, 
                                    times = N)

train<- train.N[,1]

# all rows except for training
test <- setdiff(1:nrow(df),train)

# half of the test set , stratified
val <- test[createDataPartition(df[test,]$CAD, list=TRUE)[[1]]] # times=1 , p=0.5 are  defaults

# remaining half of the test set
test<-setdiff(test,val)

checkPartition(train,val,test, df)

doc4 <- data.frame()

N<- 100
for(i in 1:N){
  
  rg <- ranger(CAD~.
               , data = df[train,]
               #, num.trees = 23
               , num.trees=500
               , importance='impurity'
               #, mtry = 3
  )
  
  pv<-predict(rg$forest,df[val,])$predictions %>% acc(.,df[val,'CAD'])
  pt<-predict(rg$forest,df[test,])$predictions %>% acc(.,df[test,'CAD'])
  
  new.row<-c(pv,pt)
  doc4<-rbind(doc4,new.row)
}
names(doc4)<-c('acc.val','acc.test')
xgrid<-1:nrow(doc4)

plot(x=xgrid 
     , y=doc4[,1]
     , main='accuracy on validation and test set\n(different forests, same data partitions)'
     , xlim=c(1,nrow(doc4))
     , ylim=c(min(doc4[c(1,2)]) , max(doc4[c(1,2)]))
     , ylab='accuracy'
     , xlab='forest'
)

points(x=xgrid , y=doc4[,2] , col='blue')
legend('topright', legend=c('val','test') , col=c('black','blue'), pch='o')
# this is a clear statement: The difference depends more on the data partition than on the tree!
t.test(doc4[,1],doc4[,2]) # p-value almost 0 , sample estimates for the mean differ by almost 9%
# in detail:
mean(abs(doc4[,1]-doc4[,2]))
sd(abs(doc4[,1]-doc4[,2]))
summary(abs(doc4[,1]-doc4[,2]))

plot(x=xgrid
     , y=doc4[,1]-doc4[,2]
     , ylim=c(-0.13,0.01)
     , xlab='forest'
     , ylab='difference in accuracy'
     , main='difference in accuracies on val and test set\n(different forest, same data partitions)')
abline(h=0, col='grey')

# If the difference in accuracy on validation and test data is more random over the choice of val and test sets (than on the choice of trees)
# then we should generally do cross validation over the data partitions. 
# Instead of having a single validation set we (always) need a rotating validation set.
# This implies that the training data rotates, too, and a new forest build each time.

plot(x=jitter(doc4[,1])
     , y=jitter(doc4[,2])
     , xlim=c(0.5,1)
     , ylim=c(0.5,1)
     , xlab='val accuracy'
     , ylab='test accuracy'
     , main='correlation of val and test accuracy\n(different forest, same data partitions)')
abline(a=0, b=1, col='grey') # what I would like to see
doc4.lm.1<-lm(acc.test~0+acc.val , data=doc4) # 1 parameter regression line (only slope)
abline(doc4.lm.1, lty=32)
doc4.lm.2<-lm(acc.test~acc.val , data=doc4) # 2 parameter regression line (intercept and slope)
abline(doc4.lm.2, lty=3)
# compared to the previous plot, this one has a much smaller cloud / much smaller diameter
# this is: the forest introduces much less variation / is much less important than the distribution of the val / test set.
summary(doc4.lm.1) # much smaller residual std error:  2%
