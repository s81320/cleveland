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

{dataGen02$info
N <- dataGen02$N
df<- dataGen02$df 
splitN <- dataGen02$splitN
rangerN <- dataGen02$rangerN
dmN <- dataGen02$dmN
acc.ff <- dataGen02$acc.ff
acc.oob <- dataGen02$acc.oob
}

########################################################
#### baseline : random sampling ########################
#### new: collect info on tree distance and oob acc ####
########################################################

#size.sf<-c(3,5)
size.sf<-c(3,5,7,11,13,17)
#size.sf<-c(10,20,30,40,50)

nRS<-10 # number of random samples per full forest 
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
rs[[N]][[nRS]] # 1st index for repetition , 2nd for the partition

# samples are in sequential order of trees , not exactly what we want. We create permutations to remove the ordering
for(i in 1:length(rs)){
  for(j in 1:length(rs[[1]])){
    rs[[i]][[j]]<- sample(rs[[i]][[j]])
  }
}

# samples for different sizes will be from the same partition
# directly ...
# uncomment to check how rs and size.sf work toghether
#s<-2
#rs[[1]][[1]][1:size.sf[s]]
## ... or vectorised
#a<-Vectorize(function(s)rs[[1]][[1]][1:s])
#a(size.sf)
## results in nested samples

#### for each subforest calculate ########################################
#### accuracies on validation set , mean oob accuracy , mean distance ####
#### results : acc.sf , moa.sf , md.sf ###################################
##########################################################################

acc.sf <- array(0, dim=c(length(size.sf), nRS , N)) # accuracy subforest
moa.sf <- array(0, dim=c(length(size.sf), nRS , N)) # mean oob accuracy of trees in sub-forest
md.sf  <- array(0, dim=c(length(size.sf), nRS , N)) # mean distance among trees in sub-forest

for(i in 1:N){
  forest <- rangerN[[i]]$forest
  val <- splitN$val[[i]]
  metric<-'d0'
  dm <- dmN[[metric]][[i]]
  ao <- acc.oob[[i]]
  
  for(j in 1:nRS){
    trindcs.all<-rs[[i]][[j]] # this is a vector, we only need the first size.sf[k] entries
    
    for(k in 1:length(size.sf)){
      trindcs<-trindcs.all[1:size.sf[k]]
      
      apsf(forest, trindcs, df[val,]) %>% 
        round(5) -> 
        acc.sf[k,j,i]
      
      mean(ao[trindcs]) %>% 
        round(5) ->
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

baseCase.rs.sf<- list(info=paste('created with : dataEval-baseCase-rs-03.R'
                                 , '\n date : ',Sys.time()
                                 , '\n content :  '
                                 , '\n * rs : random samples of tree indices' 
                                 , '\n the first 3,5,7... of each sample define the subforest of the desired size, subforests are nested'
                                 , '\n * size.sf : for the subforest sizes used in the following'
                                 , '\n * acc.sf : accuracy of each sub-forest'
                                 , '\n * moa.sf : mean oob accuracy for each sub-forest'
                                 , '\n * md.sf : mean dissimilarity for each subforest (only one dissimilarity used)'
                                 , '\n * metric : the dissimilarity metric used in md.sf')
                      , content = c('rs','size.sf','acc.sf','moa.sf','md.sf','metric')
                      , rs=rs
                      , size.sf=size.sf
                      , acc.sf=acc.sf
                      , moa.sf=moa.sf
                      , md.sf=md.sf
                      , metric=metric
                      )

file='data/10forests/baseCase-rs-sf.rda'
#save(baseCase.rs.sf , file=file)
#load(file)
#baseCase.rs.sf$info %>% cat()
#baseCase.rs.sf$content

##### turn acc.sf, moa.sf ,md.sf into a tidy data table ####
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
td$acc.ratio <- td$acc.sf / td$acc.ff %>% round(5)

write.csv(td , "data/td.csv", row.names = FALSE)

#### do other things ####
#########################

source('code/basecases/rs/source03/generate-and-viz-statistics-on-td.R')
source('code/basecases/rs/source03/viz-td.R')
source('code/basecases/rs/source03/linear-models.R')
source('code/basecases/rs/source03/viz-rs-sf-in-treespace.R')

