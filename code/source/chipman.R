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
  print(paste('cutoff: ' , cutoff, is.na(cutoff)))
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
  
  if(!is.na(cutoff)){
    sT<- c(1) # _s_elected _T_rees : we start selecting only tree 1
    cT<-c() # -c_losetst _T_rees : no tree is closest to the tree(s) selected up to now
    for(tj in 2:ncol(D)){ # go through all trees not yet selected, column index of distance matrix
      print(paste('tree : ' , tj))
      print(paste('selected : ', list(sT)))
      print(paste('distance : ', D[sT,tj]))
      print(D[1:4,1:4])
      if(min(D[sT,tj])>cutoff){
        print('calling which')
        print(which(D[sT,tj]==min(D[sT,tj]),arr.ind=TRUE)[[1]])
        cT<- c(cT
               ,which(D[sT,tj]==min(D[sT,tj]),arr.ind=TRUE)[[1]] %>% sT[.] %>% list()
               # which returns indices relative to the selected Trees in sT
               )
        sT<- c(sT,tj) # update _after_ ct has been set
        }
    }
  }
  
  D<-D[,-1] # remove 1st column : it is the distance of the 1st tree to the 1st tree (=0)
  # D is no longer square , ncol(D) +1 = nrow(D)
  # column indices refer to trees (that might be added to the first tree: 1st col for 2nd tree, 2nd col for 3rd tree, ...)
  # row indices refer to the original trees 1,2,...
  
  sdist<-apply(D,2,min) # smallest distance (to closest tree for added tree) for trees 2,3,...
  
  result<- list(smallestDist=c(NA,sdist)) # starts with NA , because the first tree is not closest to any other previously added tree
  if(!is.na(cutoff)){ 
    result$closestTree=cT # indices relate to trees ranging 1,2,.. as ordered in rg$forest
    result$selectTrees=sT # indices relate to trees ranging 1,2,.. as ordered in rg$forest
  }
  return(result)
}

ctplot<- function(D, cutoff=NA, returnTrees=FALSE){
  # closest tree plot is a reduced version of added tree plot to select a spanning sub-forest
  
  # with returnTrees=TRUE the trees that have a smallest distance to any other 
  # previously added tree
  # exceeding the cutoff will be returned.
  
  # Chipman & friends add these trees to select a diverse set of trees, hopefully a spanning sub-forest.
  
  # D should be a distance matrix , a square matrix , nrow(D), ncol(D) > 1 but larger than that should be expected
  # objects of class dist are not accepted for now
  # to use an object of class dist
  # dm <- as.matrix(dist.obj)
  # attributes(dm)<-list(dim=c(nrow(dm),ncol(dm))) : strip rownames and colnames to work properly
  
  ctd<-ctdata(D,cutoff)
  sdist<-ctd$smallestDist
  ct<-ctd$closestTree # length(ct) = length( ctd$selectTrees) -1
  
  nT<-nrow(D) # number of trees , we will plot one less
  #print(nT)
  
  col<-rep(1,nT)
  if(!is.na(cutoff)){ col[cT[sT]]<-2 }
  
  xgrid<- 2:nT
  
  plot(x=xgrid
       , y=sdist[xgrid] # 1st entry is na , 1st tree is not closest to anything
       , xlim=c(2,nT)
       , xlab='added tree'
       , ylim=c(0,min(max(sdist, na.rm = TRUE)+1,max(sdist, na.rm = TRUE)*1.05))
       , ylab='smallest distance to previoulsly added trees'
       , col=col[xgrid]
       , main='added tree plot: closest tree'
       )
  
  if(!is.na(cutoff)){ abline(h=cutoff, col='grey') }
  
  labels<-c(rep('',times=nT-1)) # count 2,3,.., nT , 1 for it is closest to the 2nd tree , which comes first
  labels[ctd$selectTrees[-1]-1]<-ct # -1 in selectTrees since we do not plot the first tree
                                    # -1 outside of selectTrees because ...
  # print(labels)
  text(x=xgrid
       , y=sdist[xgrid]
       , pos=3
       , labels=labels
       )
  # Left out trees do not get a label.
  # I should consider not plotting left out trees.
  # labels should be lists when more than one tree is closest (will happen often for d0 metric)
  
  if(returnTrees){return(ctd$selectTrees)}
}

chipmanAccRatios<- function(metric, cutoffs, cluster.k, forest, dm2, dfv){
  # dfv are the observations in the validation set, not just indices
  # we need this to calculate accuracy (and accRatio) of the (clustered) subforest
  
  # return value
  # a data frame
  
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
