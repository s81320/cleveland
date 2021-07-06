# in this script we avoid chipman
# we cluster the forests directly

# the basecase is to select randomly from the full forest.
# here we explore the strategy to cluster and then create a subforest as the set of medoid trees

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
load('code/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

## accuracy of the full forest on the validation sets ##
aff<-c(rep(0,N))
for(i in 1:N) aff[i]<-docN[[i]]$accuracy[1]
boxplot(aff, 
        main=paste('accuracy full forest, ',N,'repetitons\n(mean:', mean(aff) %>% round(3) , ', sd:', sd(aff) %>% round(3),')'))
summary(aff)
# focus on variation , measured by standard deviation
mean(aff) ; sd(aff)

############################################################
#### build a subforest from clustering (choose medoids) ####
############################################################
#### result : doc.sf.clus ##################################
############################################################

doc<-data.frame()

metrices <- c('d0','d1','d2','sb')
#cluster.k<-c(10,20,30,40,50)
cluster.k<-c(3,5,7,11,13)
# cluster.k<-c(10,30)

for(i in 1:N){
  if(i==1) print(paste('For each metric clustering into different number of clusters and calculating accuracy ratios on validation set'))
  print(paste(i,'of',N))
  
  doc.i<-data.frame()
  rg<-docN[[i]]$ranger
  val<-docN[[i]]$val
  accff<-docN[[i]]$accuracy[[1]]
  
  for(metric in metrices){
    
    dm.m<-docN[[i]]$distMatrices[[metric]]
    doc.m<-data.frame()
    
    for(k in cluster.k){
      pam.obj <- cluster::pam(dm.m
                            , k=k
                            , diss=TRUE
                            , medoids='random'
                            , nstart = 5)
  
      acc.sf.pam <- apsf(rg$forest, pam.obj$medoids, df[val,])
      
      new.row <- data.frame( 'acc.ff'=accff
                    , 'metric'=metric # character
                    , 'num.cluster'=k
                    , 'avg.sil.width'=pam.obj$silinfo$avg.width
                    , 'perc.pos.sil.width'=as.vector(table(factor(pam.obj$silinfo$clus.avg.widths>0, levels=c(FALSE,TRUE))))[[2]] / k # % of clusters with positive avg width
                    , 'acc.sf.pam'=acc.sf.pam  %>% round(5)
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

#file<-'code/basecases/clusterMedoids/subforest-clustering-from-500trees.rda'
#save(doc.sf.clus, file=file)
#load(file)

doc.sf.clus %>% 
  group_by(metric,num.cluster) %>% 
  summarise( m=mean(accRatio) )

doc.sf.clus %>% 
  group_by(num.cluster) %>% 
  summarise( m=mean(accRatio) )

# remove distance d1 
doc.sf.clus[doc.sf.clus$metric!='d1',]  %>% 
  group_by(num.cluster) %>% 
  summarise( m=mean(accRatio) )

#######################################################
## relevance of clustering quality  ###################
#######################################################

cor(x=doc.sf.clus$avg.sil.width , y=doc.sf.clus$accRatio)
cor(x=doc.sf.clus$perc.pos.sil.width , y=doc.sf.clus$accRatio)

# result / interpretation:
# in general: indicators of good clustering quality 
# do not correlate with high accuracy ratios
#######################################################

lm.obj.1<-lm(accRatio~. , data=doc.sf.clus)
summary(lm.obj.1)
# since accRatio = acc.sf.pam / acc.ff we get R-squared almost 1 if we include this information
# we have a quotient and the linear model can be fit to it with small p-values and large R-squared?
# interesting... but not expected ... there must be small variation in the denominator?!

# we include only variables that we control: the metric and the number of clusters 
# no intercept
lm.obj.2<- lm(accRatio~0+metric+num.cluster , data=doc.sf.clus)
summary(lm.obj.2)
# each metric has its own estimated (highly) significant accRatio (small p-values)
# and number of clusters is also (very) significant, tending towards larger clusters
# R squared almost 1 , all variance explained

# check if interactions of metric and number of clusters are significant ...
lm.obj.3<- lm(accRatio~0+metric+num.cluster+metric*num.cluster , data=doc.sf.clus)
summary(lm.obj.3)
anova(lm.obj.2 , lm.obj.3, test="Chisq")
# ... they are not. We stick with the simpler model of metric and num.cluster, without interactions

# including silouhette related features as predictors
lm.obj.4<- lm(accRatio~0+metric+num.cluster+avg.sil.width+perc.pos.sil.with, data=doc.sf.clus)
summary(lm.obj.4)
# the p value for number of clusters increases when adding avg.sil.width (and per.pos.sil.width)
# silhouette related features (and num.clusters) have no significant influence (large p-values)
# coefficients for silhouette related features have positive coefficients, as expected.

cor(doc.sf.clus$perc.pos.sil.width , doc.sf.clus$avg.sil.width)
cor(doc.sf.clus$num.cluster , doc.sf.clus$avg.sil.width)
#######################################################################################################
#### Get expected accuracy ratios for the different combinations of metric and numbers of clusters ####
#######################################################################################################
#### we only work with the linear model 2 lm.obj.2

newdata<-data.frame(matrix(0, nrow=20, ncol=6))
names(newdata)<-names(doc.sf.clus)[-ncol(doc.sf.clus)]

newdata[,c('metric','num.cluster')]<-expand.grid(x=metrices
                                                 , y=cluster.k)
cbind(newdata[,c('metric','num.cluster')]
      ,accRatio=predict(lm.obj.2, newdata=newdata))

# and chose parameters for best (expected) outcome of the (target) accuracy ratio.

#################################################################
#### test it ! on the test set! #################################
#################################################################

num.cluster<-13 # optimal clustering parameter
doc<- data.frame()

for(i in 1:N){
  if(i==1) print(paste('testing clustering into ' , num.cluster , ' clusters and calculating accuracy ratios on test set'))
  print(paste(i,'of',N))
  
  doc.i <- data.frame()
  forest <- docN[[i]]$ranger$forest
  test <- docN[[i]]$test
  accff <- docN[[i]]$accuracy[2] # test accuracy for the full forest on the test set!!
  
  doc.m<-data.frame()
  for(metric in metrices){
    # cluster
    dm.m <- docN[[i]]$distMatrices[[metric]]
    pam.obj <- cluster::pam(dm.m
                              , k=num.cluster # optimal parameter
                              , diss=TRUE
                              , medoids='random'
                              , nstart = 10)
    # evaluate accuracy 
    accsf<-apsf(rg$forest, pam.obj$medoids, df[test,]) 
    
    # document
    new.row <- data.frame( 'accff'=accff
                  , 'metric'=metric
                  , 'num.cluster'=num.cluster
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

mean(doc.tested$accRatio)

doc.tested %>% 
  group_by(metric) %>% 
  summarise( min=min(accRatio), mean=mean(accRatio), median = median(accRatio), max=max(accRatio), sd= sd(accRatio))

# 1st reaction:
# this looks like a success for clustering / sub-forest of cluster medoids

# maybe less enthusiastic:
# the test set seems to be favorable, check out the same table for data from the validation set
# here the results are okay-ish and not as good.

doc.sf.clus %>%
  filter(num.cluster==13) %>%
  group_by(metric) %>% 
  summarise( min=min(accRatio), mean=mean(accRatio), median = median(accRatio), max=max(accRatio), sd= sd(accRatio))

