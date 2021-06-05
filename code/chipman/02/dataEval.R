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

load('doc-500trees-10rep.rda')

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

