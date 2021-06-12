rm(list=ls())
setwd("~/Documents/ds/sem4/thesis/trees-02-ranger/04-chipman")

library(dplyr)
library(ranger)
library(ggplot2)

# source('distance-matrices-02.R') # my own code
source('subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('chipman.R')
source('helper-functions.R')

# load df (with factors) 
# load docN with N repetitions for 
## data splits and the ranger random forest build on the current train set
load('doc-500trees-10rep-5maxDepth.rda') # adds df (cleveland data set) and docN (10 times: datasplit, RF, distance matrices)
N <- length(docN)

##############################################
#### chipman sub-forests' accuracy ratios ####
##############################################

metrices <- c('d0','d1','d2','sb')

# cutoffs for forest with rg$num.trees==500
cutoffs<-list('d0' = c(0.05, 0.15) 
              , 'd1' = c(5500, 6000, 6500,7000)
              , 'd2' = c(44,46,48)
              , 'sb' = c(17,18,19,20))
# how to automate this?
# cutoffs to select fixed numbers of trees? 1%, 3% ,5%, 10%
# for rg$num.trees==500 this would be 5, 15, 25, 50

#cluster.k<-c(5,10,15,20,25)
cluster.k<-c(3, 5, 7, 11, 13)


cAR<-data.frame()
for(i in 1:N){
  if(i==1) print('creating cAR (chipman accuracy ratios) based on subforests built with the chipman approach')
  print(paste(i,'of',N))
  rg <- docN[[i]]$ranger
  val<- docN[[i]]$val

  for(metric in metrices){
    dm <- docN[[i]]$distMatrices[[metric]]
   
    accff<-docN[[i]]$accuracy[[1]]

    ca<-chipmanAccRatios(metric=metric
                           , cutoffs=cutoffs[[metric]]
                           , cluster.k=cluster.k
                           , forest=rg$forest
                           , dm2=dm
                           , dfv=df[val,])
    
    cAR<-rbind(cAR
               , cbind(metric=factor(metric, levels = metrices)
                       ,ca))
   }
}

######################################################
#### cAR is created #### now come interpretations ####
######################################################

#############################################################
#### accuracy ratios for the sub-forest of diverse trees ####
#############################################################

# this is promising
## using the subforest of diverse trees , no clustering
mean(cAR$accRatio.sf)
median(cAR$accRatio.sf)
# if we can identify obvious low-performers , we are good
mean(cAR$accRatio.sf.pam.ff)
median(cAR$accRatio.sf.pam.ff)
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
# mean / median accuracy ratios  are close to 1 , exceeding 1 only once
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
# result: sub-forests are too big
# We want to reduce to 1% or 5% of the trees of the full forest

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

# interpretation / results:
# the diverse sub-forest built from d2 tends to perform better when its size is larger (not desirable)
# diverse subforests built from metrices d0, d1, sb are indifferent, which is good for our purpose

# d1 and sb have a slight tendency to perform better 
# when the accuracy of the full forest is small to begin with
# and to perform worse when the full forest performs well
## this is not desirable

# sub-forests built with d0 and d2 perform quite independently 
# from the accuracy of the full forest

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

################################################################################
#### reduce the size of the sub-forest of diverse trees by lowering cutoffs ####
################################################################################

# yet to come