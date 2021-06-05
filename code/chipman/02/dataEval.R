rm(list=ls())
setwd("~/Documents/ds/sem4/thesis/trees-02-ranger/04-chipman")

# source('distance-matrices-02.R') # my own code
source('subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('chipman-plots.R')
source('helper-functions.R')

df <- read.csv('../Cleveland/Cleve.data.csv')[-1] # not elegant, but csv reads an empty first column as X

df$Sex<-as.factor(df$Sex)
df$Chestpaintype<-as.factor(df$Chestpaintype)
df$HighFastBloodSugar<-as.factor(df$HighFastBloodSugar)
df$RestingECG<-as.factor(df$RestingECG)
df$ExInducedAngina<-as.factor(df$ExInducedAngina)
df$CAD<-as.factor(df$CAD)

dim(df) # 303 , 11 with 10 predictor variables
head(df)
table(is.na(df)) # quick check for missing data

chipmanAccRatios<- function(metric, cutoffs, cluster.k, forest, dm2, dfv){
  # dfv are the observations in the validation set, not just indices
  # we need this to calculate accuracy (and accRatio) of the (clustered) subforest
  
  doc2<-data.frame()
  #print(metric)
  for(cutoff in cutoffs){
    #print(cutoff)
    div.added.trees <- ctdata(dm2,cutoff)$selectTrees
    #print(div.added.trees)
    size.sf<-length(div.added.trees)
    
    orig.trees <- c(1,div.added.trees)
    # the first tree is the initial tree to which we add trees, it is not included in the indices about trees we select which range 2,3,4,...
    # this is pretty arbitrary, always including tree nr 1.
    # it made sense (in chipman) when trees were pruned (and a little robust) 
    # and ordered by performance (accuracy) on the validation set
    
    #print('selected trees')
    #print(orig.trees)
    acc.sf <- apsf(forest, orig.trees, df[val,]) # a p sf : accuracy prediction sub-forest
  
    for(k in cluster.k){
      if(k<size.sf){
        # cluster the selected trees
        pam.obj <- cluster::pam(dm2[orig.trees,orig.trees] 
                          , k=k
                          , diss=TRUE
                          , medoids='random'
                          , nstart = 5)
  
        acc.sf.pam <- apsf(forest, pam.obj$medoids, dfv)
  
        # do it differently:
        # append rows of numeric values , in the very end add a column with the string for the metric? or make the metric a number : 0 instead of d0...
        new.row <- c( accff
                  , cutoff
                  , size.sf
                  , acc.sf %>% round(4)
                  , ( acc.sf / accff ) %>% round(4) 
                  , k
                  , acc.sf.pam  %>% round(4)
                  , ( acc.sf.pam / acc.sf ) %>% round(4) 
                  , ( acc.sf.pam / accff ) %>% round(4) 
                  ) 
    
        doc2 <- rbind(doc2, new.row)
        
      }else{
        if(k == min(cluster.k)){ message(paste('no clustering for cutoff', cutoff, 'using metric' , metric, '. Subforest too small with size', size.sf, '.')) }
        break}
    }
  }
  if(nrow(doc2)>0){
    names(doc2) <- c('acc.ff','cutoff','size.sf','acc.sf','accRatio.sf','size.sf.pam','acc.sf.pam','accRatio.sf.pam.sf','accRatio.sf.pam.ff')
    # set metric as attribute
    attr(doc2,'metric')<- metric
  }
  
return(doc2)
  }

load('doc-500trees.rda') # or whatever data was generated 

cAR<-list()

N <- length(docN)

#docN <- list(list(ranger=rg, distMatrices=doc),list(ranger=rg, distMatrices=doc))


for(i in 1:N){
  #i<-1
  print(i)
  rg <- docN[[i]]$ranger
  val<- docN[[i]]$val

  metrices <- c('d0','d1','d2','sb')
 
  cutoffs<-list('d0' = c(0.05, 0.15) 
              , 'd1' = c(3000, 3200, 3400, 3600, 3800)
              , 'd2' = c(28, 30, 32, 34, 36)
              , 'sb' = c(15,20,25))

  for(metric in metrices){
    dm <- docN[[i]]$distMatrices[[metric]]
   
    accff<-docN[[i]]$accuracy[[1]]

    #cAR[[i]][[metric]]<-
      cAR[[metric]] <- rbind(
        cAR[[metric]],
        chipmanAccRatios(metric = metric
                   , cutoffs=cutoffs[[metric]]
                   , cluster.k = c(1,5,10,15,20)
                   , forest=rg$forest
                   , dm2=dm
                   , dfv=df[val,])
      )
   }

}

# accuracy of the validation set (by full forest)
# run repeatedly : get different plots ?? How can this even be??
aff<-c(rep(0,N))
for(i in 1:N) aff[i]<-docN[[i]]$accuracy[1]
boxplot(aff, main='accuracy full forest')

for(metric in metrices){
  print(metric)
  cAR[[metric]]$accRatio.sf %>% 
    boxplot(main=paste('accuracy ratios sf ', metric))
 abline(a=1, b=0, col='grey')

   cAR[[metric]]$accRatio.sf.pam.ff %>% 
      boxplot(main=paste('accuracy ratios clustered sf ', metric))
  abline(a=1,b=0, col='grey')
}

metrices  

