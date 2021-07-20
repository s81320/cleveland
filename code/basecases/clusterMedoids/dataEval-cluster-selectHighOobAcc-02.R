# the 1st basecase is to select randomly from the full forest.

# here, in the 2nd basecase, we explore the strategy to cluster and then create a subforest as the set of medoid trees
# we initially exclude trees with a low oob accuracy

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
  val<-docN[[i]]$val # validation set in current split
  accff<-docN[[i]]$accuracy['val']
  
  # trees to keep
  # select high performers (high oob acc)
  trindcs <- order(acc.oob[i,])[351:500]
  # for testing : keep all trees!
  #trindcs <- 1:500
  
  # modify forest
  forest <- subforest(docN[[i]]$ranger$forest , trindcs)
  
  for(metric in metrices){
    
    dm.m<-docN[[i]]$distMatrices[[metric]]
    
    # next line creates confusion . If the results do not reflect this (by getting worse) then there is something wrong
    #dm.m<-dm.m[500:1,500:1] # reverse order, this should confuse things ... but if it is random anyway then it does not matter??
    
    # modify distance matrix
    dm.m<-dm.m[trindcs,trindcs]
    
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

load("data/10forests/baseCase02-rs-500trees.rda")
baseCase02$info

baseCase02$stats$mean.acc.ratio

# accuracy ratio:
# distance matrix confused
#arc<-doc.sf.clus %>% 
#  group_by(num.cluster) %>% 
#  summarise( m=mean(accRatio) , sd=sd(accRatio))

# when keeping all trees, this is the clean clustering and selecting the medoids
arc.all.medoids<-doc.sf.clus %>% 
  group_by(num.cluster) %>% 
  summarise( m=mean(accRatio) , sd=sd(accRatio))

# accuracy ratio clustered when previously selected th high performers (high oob acc on training)
arc.hp.medoids<-doc.sf.clus %>% 
  group_by(num.cluster) %>% 
  summarise( m=mean(accRatio) , sd=sd(accRatio))

# accuracy ratio clustered when previously selected th high performers (high oob acc on training)
arc.confused<-doc.sf.clus %>% 
  group_by(num.cluster) %>% 
  summarise( m=mean(accRatio) , sd=sd(accRatio))

# disappointing?
xgrid<-cluster.k
plot(x=xgrid
     , y=baseCase02$stats$mean.acc.ratio[1:5] 
     , type='b'
     , ylim=c(0.94,1)
     , main='base cases'
     , xlab='forest size'
     , ylab='accuracy ratio'
     )
points(xgrid, arc.confused$m, type = 'b' , col='green') # pam sub-forest, confused dissimilarity , 500:1 instead of 1:500
points(xgrid, arc.all.medoids$m, type = 'b' , col='blue') # pam sub-forest, clustered from full forest
points(xgrid, arc.hp.medoids$m, type = 'b' , col='red') # pam sub-forest, low ooc acc trees removed, size of forest halved
legend("bottomright" 
       , legend=c("random sampling", "cluster medoids, confused", "cluster medoids","cluster medoids, hp")
       , col=c('black','green','blue','red')
       , pch='o'
       , cex=0.8) 

# clustered high performers are always above random sampling
# not by much . and not close to 1 . well ...

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
