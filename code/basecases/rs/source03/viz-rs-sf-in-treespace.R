# source this at the end of dataEval-baseCase-rs-03.R t

# needs loaded data at the beginning of dataEval-baseCase-rs-03.R
#### to load the required data : uncomment
# load list dataGen02 with info
#load('data/10forests/500trees-10rep-5maxDepth-keepInbag.rda') 
#dataGen02$info
#dmN <- dataGen02$dmN
#acc.ff<-dataGen02$acc.ff

# needs randomly sampled sub forests rs
#load('data/10forests/baseCase-rs-sf.rda')
#baseCase.rs.sf$info %>% cat()
#baseCase.rs.sf$content
#rs<-baseCase.rs.sf$rs
#acc.sf<-baseCase.rs.sf$acc.sf

###################################################
#### visualize the selected trees in treespace ####
###################################################

i<-1 # for the repetition , which data split and forest # no looping yet

D <- dmN$d1[[i]]
# dim(D)

# we need acc.sf a matrix generated in dataEval-baseCase-rs-03.R
# but not saved as an rda file...

m <- cmdscale(D, eig = TRUE, k = 2) # this is created once and for the full forest.
# of the x,y vectors subsets will be used for the selected trees.
x <- m$points[, 1]
y <- m$points[, 2]

size.sf<-c(3,5,7,11,13,17)

for(j in 1:2){ # look at 3 repetitions
  xlim<-c(min(x[rs[[i]][[j]][1:max(size.sf)]])
          ,max(x[rs[[i]][[j]][1:max(size.sf)]]))
  ylim<-c(min(y[rs[[i]][[j]][1:max(size.sf)]])
          ,max(y[rs[[i]][[j]][1:max(size.sf)]]))
  for(k in 1:length(size.sf)){
    s<-size.sf[k]
    trindices<-rs[[i]][[j]][1:s]
    x.plot<-x[trindices]
    y.plot<-y[trindices]
    plot(x=x.plot
        , y=y.plot
        , pch = 19
        , xlim = xlim
        , ylim= ylim
        , xlab = 'cmdscale x'
        , ylab = 'cmdscale y'
        , main=paste('i=',i,', j=',j,', size=',s,', acc=',acc.sf[k,j,i]%>%round(4) ,'acc.ratio=', (acc.sf[k,j,i]/acc.ff$val[[i]])%>%round(4) ))
  }
}


###########################################################################
#### Look at how the accuracy , accuracy ratio evolves as we add trees ####
###########################################################################

J<-3 # limit for j , j will be in 1:J
# use x=size.sf , y=data to be plotted to get the x axis labels right...
plot(x=size.sf
     , y=acc.sf[,1,i]
     , type='b'
     , ylim=c(0.6,0.9)
     , xlab='forest sizes'
     , ylab='accuracy'
     , main='randomly sampled subforests, size increasing\n(nested subforests, increase by adding trees)')
legend('bottomright',legend=paste('j=',c(1:J)), col=c(1:J),pch='o')
for(j in 2:J){
  points(x=size.sf , y=acc.sf[,j,i], type='b', col=j)
}


# can we get an indicator: when does the plot line increase, when does it decrease?
# depending on properties of the added trees (in relation to the already present / selected trees)?

###########################################################################
#### remove trees from a forest ###########################################
###########################################################################

# sub-forest of 7 trees. which is the best tree to remove? ####
###############################################################

# check this works
# apsf(forest=rangerN[[1]]$forest , idx=(1:7) , newdata=df[val,])

m<-1
set.seed(0)
for(m in 1:8){
  apsf(forest=rangerN[[1]]$forest , idx=(1:8)[-m] , newdata=df[val,]) %>% print()
}
# removing a tree from a prime size subforest results in an even number of trees with the posibility of ties and required randomness (to resolve the tie)
# this is reduced when setting a seed.
# it vanishes when starting with a number of trees s.th. having one tree less, we get a prime number of trees.
# then there will be no ties and no randomness.

# this does as expected, and is the same as the above loop ... whenever there is no tie. See above.
Vectorize(function(m)apsf(forest=rangerN[[1]]$forest , idx=(1:8)[-m] , newdata=df[val,]))(1:8)

# get the tree the to remove in one line of code :-)
which.max(Vectorize(function(m)apsf(forest=rangerN[[1]]$forest , idx=(1:8)[-m] , newdata=df[val,]))(1:8))

