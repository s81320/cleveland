# based on the Chipman, Geaorge, McCulloch 1998 paper

# mds plot
mds<-function(D){
  m <- cmdscale(D, eig = TRUE, k = 2)
  x <- m$points[, 1]
  y <- m$points[, 2]

  plot(x, y, pch = 19, xlim = range(x) , main='mds')
  text(x, y, pos = 4, labels =1:nrow(D))
}

mds2<-function(D){
  m <- cmdscale(D, eig = TRUE, k = 2)
  x <- m$points[, 1]
  y <- m$points[, 2]
  
  k<-nrow(mgcv::uniquecombs(m$points,ordered=FALSE))
  pam.obj<-cluster::pam(x = m$points, k = k, diss = FALSE)
  plot(x, y, pch = 19, xlim = range(x) , main='mds', col=pam.obj$clustering)
  text(x, y, pos = 4, labels =1:nrow(D))
  
  return(pam.obj$clustering)
}


# added tree plot
atp<- function(D){
  plot(x=rep(seq(2,nrow(D),1),times=1:(ncol(D)-1))# %>% jitter() # creates 2, 3,3, 4,4,4, 5,5,5,5, ...
       , y=as.vector(D[upper.tri(D)]) # %>% jitter()
       , xlim=c(2,nrow(D))
       , xlab='added tree'
       , ylim=c(0,max(as.vector(D))*1.01)
       , ylab='distance to previoulsly added trees'
       , main='added tree plot'
  )
}





# closest tree data , will be used to plot 
# the distance of the closest tree in the added tree plot

ctdata<- function(D, cutoff=NA){
  
  if('matrix' %in% class(D)){
    if(nrow(D)!=ncol(D)) message('expecting a square matrix')
  }else{message('first argument should be a (square, symmetric) matrix')}
  # to use an object of class dist
  # dm <- as.matrix(dist.obj)
  # attributes(dm)<-list(dim=c(nrow(dm),ncol(dm))) : strip rownames and colnames to work properly
  
  
  # check for symmetry (this function ignores values in the lower triangle of D) and 0-values on the diagonal
  upper.bound<-max(D)*1.01
  #print(paste('upper bound',upper.bound))
  D[lower.tri(D, diag=TRUE)] <- upper.bound
  
  D<-D[,-1] # remove 1st column : it is the distance of the 1st tree to the 1st tree (=0)
  # D is no longer square , ncol(D) +1 = nrow(D)
  # column indices refer to trees (that might be added to the first tree: 1st col for 2nd tree, 2nd col for 3rd tree, ...)
  # row indices refer to the original trees 1,2,...
  
  sdist<-apply(D,2,min) # smallest distance (to closest tree for added tree) for trees 2,3,...
  # in apply : 2 : columns index fixed, move through rows when applying min : get the min of the column going through D
  ct<-lapply(1:ncol(D), function(i) which(D[,i]==sdist[i])) # trees of minimal distance , closest tree for trees 2,3,...
  # ct[i]=j : tree j is closest to the i-th added tree, tree i+1
  
  result<- list(smallestDist=sdist
                , closestTree=ct) # indices are relative to the added trees ranging 2,3,...
  if(!is.na(cutoff)){ 
    result$selectTrees=1+which(sdist>cutoff) # due to 1+ indices relate to trees ranging 1,2,..
  }
  return(result)
}

ctplot<- function(D, cutoff=NA, returnTrees=FALSE){
  # closest tree plot is a reduced version of added tree plot to select a spanning sub-forest
  
  # with returnTrees=TRUE the trees that have a smallest distance to any other 
  # previously added tree (tree 1 for the first added tree which is tree 2)
  # exceeding the cutoff will be returned.
  
  # Chipman & friends add these trees to selevt a diverse set of trees, hopefully a spanning sub-forest.
  
  # D should be a distance matrix , a square matrix , nrow(D), ncol(D) > 1 but larger than that should be expected
  # objects of class dist are not accepted for now
  # to use an object of class dist
  # dm <- as.matrix(dist.obj)
  # attributes(dm)<-list(dim=c(nrow(dm),ncol(dm))) : strip rownames and colnames to work properly
  
  ctd<-ctdata(D,cutoff)
  sdist<-ctd$smallestDist
  ct<-ctd$closestTree
  
  nT<-length(sdist) # number of trees for which to plot
  #print(nT)
  
  col<-rep(1,nT)
  if(!is.na(cutoff)){ col[sdist>cutoff]<-2 }
  
  xgrid<- 2:(nT+1)
  
  plot(x=xgrid
       , y=sdist
       , xlim=c(1,nT)+1
       , xlab='added tree'
       , ylim=c(0,min(max(sdist)+1,max(sdist)*1.05))
       , ylab='smallest distance to previoulsly added trees'
       , col=col
       , main='added tree plot: closest tree'
       )
  
  if(!is.na(cutoff)){ abline(h=cutoff, col='grey') }
  
  text(x=xgrid
       , y=sdist
       , pos=3
       , labels=ct
       )
  
  if(returnTrees){return(ctd$selectTrees)}
}
