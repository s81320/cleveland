rm(list=ls())

library(dplyr)
library(ranger)
library(ggplot2)

# my own code
source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/chipman.R')
source('code/source/helper-functions.R')

# load df (with factors) 
# load docN with N repetitions for 
## data splits and the ranger random forest build on the current train set
load('code/doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

##############################################
#### chipman sub-forests' accuracy ratios ####
##############################################

# set the cutoff to the mean distance (or 0.9*mean distance or maybe a factor larger than 1) 
# to remove trees that are really close to some other tree
# then cluster and select the medoid into a sub-forest

metrices <- c('d0','d1','d2','sb')

#cluster.k<-c(5,10,15,20,25)
cluster.k<-c(3, 5, 7, 11, 13)

cAR <- data.frame() # start empty, built up while looping, waste time
# better: allocate space at the beginning
# cAR<-data.frame(matrix(0,nrow=N*length(metrices),ncol=10))
# cf R-inferno chapter 2

for(i in 1:N){
  if(i==1) print('creating cAR (chipman accuracy ratios) based on subforests built with the chipman approach\n cutoff based on mean distance')
  print(paste(i,'of',N))
  rg <- docN[[i]]$ranger
  val<- docN[[i]]$val
  accff<-docN[[i]]$accuracy[[1]]
  
  for(metric in metrices){
    dm <- docN[[i]]$distMatrices[[metric]]
    
    # factor 1 creates small sizes of sub-forests (before clustering)
    cutoffs <- c(0.9, 0.95, 1, 1.05)*mean(dm[upper.tri(dm)]) # specific for the mean distances for each metric and each data split (each forest built on its training data)
    
    for(cutoff in cutoffs){
      ca<-chipmanAccRatios(metric=metric
                           , cutoffs=cutoff
                           , cluster.k=cluster.k
                           , forest=rg$forest
                           , dm2=dm
                           , dfv=df[val,])
      
      # alternative: only add / rbind if ca$size.sf<51
      cAR<-rbind(cAR
                 , cbind(metric=factor(metric, levels = metrices)
                         ,ca))
    }
  }
}

# remove observations with a sub-forest size larger than 10% of the full forest (50, 500)
table(cAR$size.sf>50)
cAR <- cAR[cAR$size.sf<51,]
dim(cAR)

######################################################
#### cAR is created #### now come interpretations ####
######################################################

## 1st step : not choosing trees that are close to a previously added tree (atp)
cAR %>% 
  group_by(metric) %>% 
  summarise(mean=mean(accRatio.sf), std= sd(accRatio.sf) , size=mean(size.sf))
# this is promising!

# not grouped by metric
mean(cAR$accRatio.sf) ; median(cAR$accRatio.sf) ;  sd(cAR$accRatio.sf) ; mean(cAR$size.sf)

## 2nd step : clustering

cAR %>% 
  group_by(size.sf.pam) %>% 
  summarise(m=mean(accRatio.sf.pam.ff), std=sd(accRatio.sf.pam.ff))
# this is very similar to random sampling and simple clustering
# can't say it is better than the basecase

# not grouped by metric or num.clusters
mean(cAR$accRatio.sf.pam.ff) ; median(cAR$accRatio.sf.pam.ff) ;  sd(cAR$accRatio.sf) ; mean(cAR$size.sf.pam)
#############################################################
#### accuracy ratios for the sub-forest of diverse trees ####
#############################################################

## using the sub-forest of diverse trees , no clustering
mean(cAR$accRatio.sf)
median(cAR$accRatio.sf)
# if we can identify obvious low-performers , we are good
mean(cAR$accRatio.sf.pam.ff)
median(cAR$accRatio.sf.pam.ff)

cAR %>% 
  group_by(metric) %>% 
  summarise(m=mean(accRatio.sf), s=mean(size.sf))

cAR %>% 
  group_by(size.sf.pam) %>% 
  summarise((m=mean(accRatio.sf.pam.ff)))

###########################################################################
#### accuracy ratios for the subforests of diverse trees by sub-forest ####
###########################################################################

par(mfrow=c(1, length(metrices)))
for(met in metrices){
  cAR %>% 
    filter(metric==met) %>% 
    .$accRatio.sf %>%
    boxplot( main=paste('sf', met)
             , ylim=c(0.8,1.1) )
  abline(a=1, b=0, col='grey')
}
par(mfrow=c(1,1))

# result: 
# mean / median accuracy ratios are around 1
# there are outliers with unacceptably low accuracy ratios , the spread of accuracy ratios is quite large

###################################################
#### accuracy ratio and size of the sub-forest ####
###################################################

par(mfrow=c(1,2))
for(met in metrices){
  cAR %>% 
    filter(metric==met) -> cARm
  
  cARm %>% 
    .$accRatio.sf %>%
    boxplot(ylim=c(0.8,1.1) 
            , main=paste('sf', met)
            , ylab='accuracy ratio')
  abline(a=1, b=0, col='grey')
  
  hist(cARm$size.sf, breaks=4 , main='size of sf' , xlab='')
}
par(mfrow=c(1,1))

# so, how are the size of the sub-forest and its accuracy ratios related?
doc<- data.frame()
for(met in metrices){
  cAR %>% 
    filter(metric==met) -> cARm
  
  new.row<- data.frame('metric'=met
                       , 'cor1'=cor(cARm$size.sf , cARm$accRatio.sf)
                       , 'cor2'=cor(cARm$acc.ff , cARm$accRatio.sf))
  doc<-rbind(doc,new.row)
}
par(mfrow=c(1,2))
barplot( height= as.vector(doc$cor1), main='cor sub-forest size\nand its acc ratio')
barplot( height= as.vector(doc$cor2), main='cor acc of full forest\nand sub-forest\'s ar')
par(mfrow=c(1,1))

# reducing the size of the sub-forest can be done by clustering , as proposed by chipman
###############################
#### effects of clustering ####
###############################

par(mfrow=c(1,1))
cAR %>% 
  group_by(metric,size.sf.pam) %>% 
  summarise(meanAccRatio.sf=mean(accRatio.sf) 
            , meanAccRatio.sf.clustered=mean(accRatio.sf.pam.ff)) -> a

plot(x=NA 
     , y=NA 
     , type='n'
     , main=paste('clustered chipman')
     , xlab='number of clusters'
     , ylab='mean accuracy ratio'
     , ylim=a$meanAccRatio.sf.clustered %>% 
       (function(x)c(floor(50*min(x))/50,max(c(1,ceiling(50*max(x))/50))))
     , xlim=c(3,13)
)
for (met in metrices){
  b <- a %>% filter(metric==met)
  points(b$size.sf.pam 
       , b$meanAccRatio.sf.clustered
       , type='b',
       col=which(metrices==met))
  # axis()
} ; rm(b)
abline(a=1,b=0, col='grey')
legend('bottomright', legend =metrices , pch='o', col = 1:4 , cex=0.8)

#############################################
#### different kind o vis , same content ####
#############################################

par(mfrow=c(2,2), oma=c(1,1,1,1)-1)
#par(mfrow=c(1,1))
for(met in metrices){
  cAR %>% 
    filter(metric==met) -> cARm
  b <- cbind(s=cARm$size.sf.pam,a=cARm$accRatio.sf.pam.ff) %>% data.frame
  b$s <- as.factor(b$s) 
  
  boxplot(b$a~b$s, xlab='', ylab='', ylim=c(0.75,1.15), main=met)
  abline(a=1,b=0, col='grey')
}
par(mfrow=c(1,1))
