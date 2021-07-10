# the 1st basecase is to select randomly from the full forest.

# here, in the 2nd basecase, we explore the strategy to cluster and then create a subforest as the set of medoid trees
# further more we initially exclude trees with a low oob accuracy

rm(list=ls())

library(dplyr)
library(ranger)
library(ggplot2)

# my code
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

# load df (Cleveland data with factors) 
# load docN with N repetitions for 
## 1-3) data splits, 4) the ranger random forest build on the current train set, 5) the accuracies, 6) the distance matrices
# load acc.oob
load('data/doc-500trees-10rep-5maxDepth-keepInbag.rda')
N <- length(docN)

##############################################
#### remove trees with a low oob accuracy ####
##############################################

# general working of order
a<- acc.oob[1,1:5]
a ; order(a) ; a[order(a)]

acc.oob[1,] %>% mean
acc.oob[1,order(acc.oob[1,])][151:500] %>% mean
acc.oob[1,order(acc.oob[1,])][201:500] %>% mean

# simpler
a<- acc.oob[1,]
mean(a)
a[order(a)][151:500] %>% mean # higher than before

# keep these trees
i<-1
trindcs <- order(acc.oob[i,])#[201:500]
# trindcs # they are ordered from lowest to highest oob acc
acc.oob[i,trindcs[1]] # worst allowed oob accuracy
acc.oob[i,trindcs[length(trindcs)]] # best oob accuracy 

# reduce forest and distance matrix to only refer to trees in trindcs
dm<-docN[[i]]$distMatrices[['d0']]
dm<-dm[trindcs,trindcs]

forest <- subforest(docN[[i]]$ranger$forest , trindcs)


############################################################
#### build a subforest from clustering (choose medoids) ####
############################################################
#### result : doc.sf.clus ##################################
############################################################

#set.seed(1989) # lower accuracy ratios than random sampling
# set.seed(700) # 
set.seed(1237) # Gründung Berlins
doc<-data.frame()

metrices <- c('d0','d1','d2','sb')
# cluster.k<-c(10,20,30,40,50)
cluster.k<-c(3,5,7,11,13)
#cluster.k<-c(20,30,40)


for(i in 1:N){
  if(i==1) print(paste('For each metric clustering into different number of clusters and calculating accuracy ratios on validation set'))
  print(paste(i,'of',N))
  
  doc.i<-data.frame()
  # rg<-docN[[i]]$ranger
  val<-docN[[i]]$val
  accff<-docN[[i]]$accuracy['val']
  
  # trees to keep
  trindcs <- order(acc.oob[i,])[351:500]
  # for testing : keep all trees!
  #trindcs <- 1:500
  
  # modify forest
  forest <- subforest(docN[[i]]$ranger$forest , trindcs)
  
  for(metric in metrices){
    
    dm.m<-docN[[i]]$distMatrices[[metric]]
    # modify distance matrix
    dm.m<-dm.m[trindcs,trindcs]
    #dm.m<-dm.m[500:1,500:1] # reverse order, this should confuse things ... but if it is random anyway then it does not matter??
    doc.m<-data.frame()
    
    for(k in cluster.k){
      pam.obj <- cluster::pam(dm.m
                            , k=k
                            , diss=TRUE
                            , medoids='random'
                            , nstart = 5)
  
      acc.sf.pam <- apsf(forest, pam.obj$medoids, df[val,])
      
      acc.oob.sf.pam <- acc.oob[i,trindcs][pam.obj$medoids]
      
      d<-dm.m[pam.obj$medoids,pam.obj$medoids] # submatrix of the distance matrix, first restricted to selected tree indices then again restricted to the pam mediods, the trees in the sub-forest
      dissim.sf.pam<-d[upper.tri(d)] # just the relevant values, losing the matrix form 
      
      new.row <- data.frame( 'acc.ff'=accff
                    , 'metric'=metric # character
                    , 'num.cluster'=k
                    , 'mean.acc.oob'=mean(acc.oob.sf.pam) %>% round(5)
                    , 'sd.acc.oob'=sd(acc.oob.sf.pam) %>% round(5)
                    , 'avg.sil.width'=pam.obj$silinfo$avg.width
                    , 'perc.pos.sil.width'=as.vector(table(factor(pam.obj$silinfo$clus.avg.widths>0, levels=c(FALSE,TRUE))))[[2]] / k # % of clusters with positive avg width
                    , 'acc.sf.pam'=acc.sf.pam  %>% round(5)
                    , 'mean.dissim'=mean(dissim.sf.pam)  %>% round(5) # mean distance of sampled trees , not using mean
                    , 'sd.dissim'=sd(dissim.sf.pam)  %>% round(5) # mean distance of sampled trees , not using mean
                    , 'accRatio'=( acc.sf.pam / accff ) %>% round(5) 
        ) 
      doc.m <- rbind(doc.m, new.row)
      }
    doc.i<-rbind(doc.i, doc.m)
  }
  doc<-rbind(doc,doc.i)
}

doc$metric<-as.factor(doc$metric)

doc.sf.clus <- doc


#file<-'data/basecase-clust-hp-500trees.rda'
#save(doc.sf.clus, file=file)
#load(file)

#####################################################

#doc.sf.clus %>% 
#  group_by(metric,num.cluster) %>% 
#  summarise( m=mean(accRatio) , sd=sd(accRatio))


# compare accuracy ratios to random sampling base case : any improvement?

load("data/baseCase02-rs-500trees.rda")
baseCase02$stats$mean.acc.ratio

# accuracy ratio:
# distance matrix confused
#arc<-doc.sf.clus %>% 
#  group_by(num.cluster) %>% 
#  summarise( m=mean(accRatio) , sd=sd(accRatio))

arc3<-doc.sf.clus %>% 
  group_by(num.cluster) %>% 
  summarise( m=mean(accRatio) , sd=sd(accRatio))
arc3

# disappointing
xgrid<-cluster.k
plot(x=xgrid
     , y=baseCase02$stats$mean.acc.ratio[1:5] 
     , type='b'
     , main='base cases'
     , xlab='forest size'
     , ylab='accuracy ratio'
     )
#points(xgrid, arc$m, type = 'b' , col='green') # pam sub-forest, confused dissimilarity
points(xgrid, arc2$m, type = 'b' , col='blue') # pam sub-forest, clustered from full forest
points(xgrid, arc3$m, type = 'b' , col='red') # pam sub-forest, low ooc acc trees removed, size of forest halved
legend("bottomright" 
       , legend=c("random sampling", "cluster medoids, confused", "cluster medoids","cluster medoids, hp")
       , col=c('black','green','blue','red')
       , pch='o'
       , cex=0.8) 

# accuracy , not accuracy ratio:
doc.sf.clus %>% 
  group_by(num.cluster) %>% 
  summarise( m=mean(acc.sf.pam) , sd=sd(acc.sf.pam))

# no improventen, no high accRatio, no small sd

# check:
# how do mean and sd of disimilarities compare for
# full forest, randomly sampled subforest, pam cluster subforest

# overall mean of dissimilarities of trees in the full forests
for(metric in metrices){
  m<-0
  d<- matrix(0,nrow=10, ncol=250*499)
  #print(paste('***',metric,'***'))
  for(i in 1:N){
    dm<-docN[[i]]$distMatrices[[metric]]
    d[i,]<-dm[upper.tri(dm)]
    #print(paste(mean(d[i,] , sd(d[i,]))))
  }
  print(paste(metric , mean(d), sd(d)))
}

# mean dissimilarities of sub-forests from random sampling, only d1 metric
mean(baseCase02$rs$mean.d1.dissim) ; sd(baseCase02$rs$mean.d1.dissim)

# mean dissim of sub-forests by clustering
doc.sf.clus %>%
  group_by(metric) %>%
  summarise(dist.mean=mean(mean.dissim) , dist.sd=sd(mean.dissim))

# makes sense:
# mean, sd dissimilarities for all trees of a full forest are the largest
# mean, sd in randomly sampled subforest are smaller (only 1 metric available)
# mean, sd in pam subforest are smallest (medoids are central, never on the edge, so they are closer (on average and with less variation))

##################################################################
## relevance of clustering quality and oob acc ###################
##################################################################

cor(x=doc.sf.clus$avg.sil.width , y=doc.sf.clus$accRatio)
cor(x=doc.sf.clus$perc.pos.sil.width , y=doc.sf.clus$accRatio)

cor(x=doc.sf.clus$mean.acc.oob , y=doc.sf.clus$accRatio)

cor(x=doc.sf.clus$mean.acc.oob , y=doc.sf.clus$acc.sf.pam)
#######################################################

# first model for all features or all but the accuracy of the pam-clustered sub-forest
lm1<-lm(accRatio~0+. , data=doc.sf.clus)
lm1<-lm(accRatio~. , data=doc.sf.clus[,c(2:7,9:11)]) # remove acc.sf.pam, 
# then the accuracy of the full forest is not even significant at 5%
summary(lm1)

lm2<- lm(accRatio~1
          + metric
          + num.cluster
          + mean.acc.oob # we do not control this
          + sd.acc.oob
          + mean.dissim
          + sd.dissim
         , data=doc.sf.clus
              )
summary(lm2)

# check if interactions of metric and number of clusters / metric and mean oob acc / number of clusters and mean oob acc are significant ...
lm3<- lm(accRatio~1
         + metric
         + num.cluster
         + mean.acc.oob
         + mean.dissim 
         + num.cluster*mean.acc.oob
         + mean.dissim*metric # mean.dissim is different for the metrices, shouldn't this matter??
         , data=doc.sf.clus
)
summary(lm3)

anova(lm2 , lm3, test="Chisq")
# ... relevant interaction ?! wow...

# including silouhette related features as predictors
lm4<- lm(accRatio~1
              +metric
              +num.cluster
              +mean.acc.oob
              +mean.dissim*metric
              +avg.sil.width
              +perc.pos.sil.width
              , data=doc.sf.clus
              )
summary(lm4)

# from previous runs of model 4:
# the p value for number of clusters increases when adding avg.sil.width (and per.pos.sil.width)
# silhouette related features (and num.clusters) have no significant influence (large p-values)
# coefficients for silhouette related features have positive coefficients, as expected.

# removing the number of clusters decreases the p-value for the avg.sil.width to almost 5% ...
### end of previous runs for model 4

#######################################################################################################
#### Get expected accuracy ratios for the different combinations of metric and numbers of clusters ####
#######################################################################################################
#### we only work with the linear model 2 lm.obj.2

mean(doc.sf.clus$acc.sf.pam[doc.sf.clus$mean.acc.oob>0.73])
hist(doc.sf.clus$mean.acc.oob)
mean(acc.oob)
newdata<-data.frame(matrix(0
                           , nrow=length(metrices)*length(cluster.k)*7
                           , ncol=ncol(doc.sf.clus)-1
                           )
                    )
names(newdata)<-names(doc.sf.clus)[-ncol(doc.sf.clus)]

newdata[,c('metric','num.cluster','mean.acc.oob')]<-expand.grid('metric'=metrices
                                                 , 'num.cluster'=cluster.k
                                                 , 'mean.acc.oob'=c(70,72,74,76,78,80,82)*0.01)
cbind(newdata[,c('metric','num.cluster','mean.acc.oob')]
      ,accRatio=predict(lm.obj.2, newdata=newdata))

# this tells us what to expect given the values for metric , num.cluster and mean acc on the oob training data
# we could check the model (on test data use our prediction and compare with test data's acc ratios, calc a mse or mean abs error.)

# the general trend is: larger sizes / more trees in subforest are better
# its not like each metric has a preferred number of trees for its sub-forest, or a preferred / suiting clustering

#################################################################
#### test it ! on the test set! #################################
#################################################################

num.cluster<-13 # optimal clustering parameter
# there should be more to do than setting the number of clusters!
# we should come up with something clever :-) 
# knowing beforehand which sub forest will perform well! Only then testing these on the test set...

# just doing the cluster thing is not enough...

doc<- data.frame()

for(i in 1:N){
  if(i==1) print(paste('testing clustering into ' , num.cluster , ' clusters and calculating accuracy ratios on test set'))
  print(paste(i,'of',N))
  
  doc.i <- data.frame()
  
  # trees to keep
  # trindcs <- order(acc.oob[i,])[401:500]
  # testing
  trindcs <- 1:500
  
  # modify forest
  forest <- subforest(docN[[i]]$ranger$forest , trindcs)
  
  test <- docN[[i]]$test
  accff <- docN[[i]]$accuracy['test'] # test accuracy for the full forest on the test set!!
  
  doc.m<-data.frame()
  for(metric in metrices){
    # cluster
    dm.m <- docN[[i]]$distMatrices[[metric]]
    # modify distance matrix
    dm.m<-dm.m[trindcs,trindcs]
    
    pam.obj <- cluster::pam(dm.m
                              , k=num.cluster # optimal parameter
                              , diss=TRUE
                              , medoids='random'
                              , nstart = 10)
    # evaluate accuracy 
    accsf<-apsf(forest, pam.obj$medoids, df[test,]) 
    
    # document
    new.row <- data.frame( 'accff'=accff
                  , 'metric'=metric
                  , 'num.cluster'=num.cluster
                  , 'mean.acc.oob'=mean(acc.oob[i,trindcs][pam.obj$medoids]) %>% round(5)
                  , 'sil.avg.width'= pam.obj$silinfo$avg.width
                  , 'perc.pos.sil.width'=as.vector(table(factor(pam.obj$silinfo$clus.avg.widths>0, levels=c(FALSE,TRUE))))[[2]] / num.cluster # % of clusters with positive avg width
                  , 'accsf'= accsf %>% round(4)
                  , 'accRatio'=( accsf / accff ) %>% round(4) 
    ) 
    doc.m<- rbind(doc.m, new.row)
  }
  doc<-rbind(doc, doc.m)
}
doc.tested<- doc
# doc.tested.5clusters <- doc
rm(new.row, dm.m, doc.m, doc, doc.i , pam.obj)

mean(doc.tested$accRatio) # 98.4% not as much as I'd expected
cor(doc.tested$mean.acc.oob , doc.tested$accsf) # I do not see the predictive power of the oob accuracy on the training set
load('data/baseCase02-rs-500trees.rda')
baseCase02$stats[5,] # 99.7 % mean acc ratio with simply randomly sampling ?? Thats unbeatable!

doc.tested %>% 
  group_by(metric) %>% 
  summarise( min=min(accRatio), mean=mean(accRatio), median = median(accRatio), max=max(accRatio), sd= sd(accRatio))
# for all metrices the accuracy ratio is  worse than with random sampling

doc.sf.clus %>%
  filter(num.cluster==13) %>%
  group_by(metric) %>% 
  summarise( min=min(accRatio), mean=mean(accRatio), median = median(accRatio), max=max(accRatio), sd= sd(accRatio))

