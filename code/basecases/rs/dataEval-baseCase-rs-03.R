# different coding style 
# more sequential, less looping
# doing all sampling as one step, then proceed to next step. 
# Instead of generating one sample, working with it and then generating the next sample

# new: visualize the random selection in tree space

rm(list=ls())

library(dplyr)
library(ranger)
library(caret)

source('code/source/subforest.R') # constructors for a sub-forest (class ranger.forest) and its hull (class ranger)
source('code/source/helper-functions.R')

# load list dataGen02 with info
load('data/10forests/500trees-10rep-5maxDepth-keepInbag.rda') 

dataGen02$info
N <- dataGen02$N
df<- dataGen02$df 
splitN <- dataGen02$splitN
rangerN <- dataGen02$rangerN
dmN <- dataGen02$dmN
acc.ff <- dataGen02$acc.ff
acc.oob <- dataGen02$acc.oob

########################################################
#### baseline : random sampling ########################
#### new: collect info on tree distance and oob acc ####
########################################################

#size.sf<-c(3,5)
size.sf<-c(3,5,7,11,13,17)
#size.sf<-c(10,20,30,40,50)

nRS<-100 # number of random samples per full forest 
# will generate nRS samples in each size for the sub-forest as specified in size.sf


#### generate random samples for selecting trees ####
#### result : rs ####################################
#####################################################

rs<- list()
p<-0.1

assertthat::assert_that(p*rangerN[[1]]$num.trees >= max(size.sf)
                        , msg='p too small or the maximum size of the sub-forest too big. Or too few trees in the full forest.')

set.seed(1237)
for(i in 1:N){
rs[[i]]<- createDataPartition( y=1:500 , p=p , times =nRS )
}
# rs[[N]][[nRS]] # 1st index for repetition , 2nd for the partition

# samples for different sizes will be from the same partition
# directly ...
s<-2
rs[[1]][[1]][1:size.sf[s]]
# ... or vectorised
a<-Vectorize(function(s)rs[[1]][[1]][1:s])
a(size.sf)
# results in nested samples

#### for each subforest calculate ########################################
#### accuracies on validation set , mean oob accuracy , mean distance ####
#### results : acc.sf , moa.sf , md.sf ###################################
##########################################################################

acc.sf <- array(0, dim=c(length(size.sf), nRS , N))
moa.sf <- array(0, dim=c(length(size.sf), nRS , N))
md.sf  <- array(0, dim=c(length(size.sf), nRS , N))

for(i in 1:N){
  forest <- rangerN[[i]]$forest
  val <- splitN$val[[i]]
  dm <- dmN$d0[[i]]
  ao <- acc.oob[[i]]
  
  for(j in 1:nRS){
    trindcs.all<-rs[[i]][[j]] # this is a vector, we only need the first size.sf[k] entries
    
    for(k in 1:length(size.sf)){
      trindcs<-trindcs.all[1:size.sf[k]]
      
      apsf(forest, trindcs, df[val,]) %>% 
        round(5) -> 
        acc.sf[k,j,i]
      
      mean(ao[trindcs]) %>% 
        round(5)->
        moa.sf[k,j,i]
      
      assertthat::assert_that(size.sf[k]==length(trindcs)
                              , msg = 'size of sub-forest and length of tree indices do not match')
      (sum(dm[trindcs,trindcs])/(size.sf[k]*(size.sf[k]-1)))  %>% 
        round(5) ->
        md.sf[k,j,i]
      
    }
  }
}

#### save generated raw data ####
# save rs, acc.sf , moa.sf , md.sf

#### turn acc.sf, moa.sf ,md.sf into a tidy data table ####
#### result : td ##########################################
###########################################################

td <- data.frame(list('forest'=1,'acc.ff'=0.5,'size'=3,'acc.sf'=0.5,'moa.sf'=0.5,'md.sf'=0.5))
# dim(td)<-c(N*nRS*length(size.sf),6) # not working . Wanted to allocate space before running

ct<-1
for(i in 1:N){
  for(j in 1:nRS){
    for(k in 1:length(size.sf)){
        td[ct,]<- c(i
                    , acc.ff$val[i]
                    , size.sf[k]
                    , acc.sf[k,j,i]
                    , moa.sf[k,j,i]
                    , md.sf[k,j,i])
        ct<-ct+1
        }
    }
}

# add column for acc ratio
td$acc.ratio <- td$acc.sf / td$acc.ff

#### do other things ####
#########################

source('code/basecases/rs/source03/generate-and-viz-statistics-on-td.R')
source('code/basecases/rs/source03/viz-td.R')
source('code/basecases/rs/source03/linear-models.R')
source('code/basecases/rs/source03/viz-rs-sf-in-treespace.R')

