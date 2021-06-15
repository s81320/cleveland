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
atp<- function(D, addLabels=FALSE){
  xgrid<-rep(seq(2,nrow(D),1),times=1:(ncol(D)-1))  # creates 2, 3,3, 4,4,4, 5,5,5,5, ...
  
  plot(x=xgrid # %>% jitter()
       , y=as.vector(D[upper.tri(D)]) # %>% jitter()
       , xlim=c(2,nrow(D))
       , xlab='added tree'
       , ylim=c(0,max(as.vector(D))*1.01)
       , ylab='distance to previoulsly added trees'
       , main='added tree plot'
  )
  
  if(addLabels)
    text(x=xgrid
         , y=as.vector(D[upper.tri(D)])
         , pos=4
         , labels=sequence(1:(nrow(D)-1))
    )
}


# closest tree data , will be used to plot 
# the distance of the closest tree in the added tree plot

ctdata<- function(D, cutoff=NA){
  #print(paste('cutoff: ' , cutoff, is.na(cutoff)))
  if('matrix' %in% class(D)){
    if(nrow(D)!=ncol(D)) message('expecting a square matrix')
  }else{message('first argument should be a (square, symmetric) matrix')}
  # to use an object of class dist
  # dm <- as.matrix(dist.obj)
  # attributes(dm)<-list(dim=c(nrow(dm),ncol(dm))) : strip rownames and colnames to work properly
  
  nT<-nrow(D) # number of trees
  
  # check for symmetry (this function ignores values in the lower triangle of D) and 0-values on the diagonal
  upper.bound<-max(D)*1.01
  #print(paste('upper bound',upper.bound))
  D[lower.tri(D, diag=TRUE)] <- upper.bound
  
  result<- list(smallestDist=apply(D[,-1],2,min)) # applying to D with 1st column removed returns smallest distance for any potentially added tree, leaving out the 1st tree, tree 1
  
  if(is.na(cutoff)){
    # pass
    }else{
      sT<- c(1) # _s_elected _T_rees : we start selecting only tree 1
      cT<-c() # -c_losetst _T_rees : no tree is closest to the tree(s) selected up to now
      sdist<-c()

      for(tj in 2:ncol(D)){ # go through all trees not yet selected, column index of distance matrix
        #print(paste('tree : ' , tj))
        #print(paste('selected : ', sT))
        #print(paste('distance : ', D[sT,tj]))
        #print(D[1:4,1:4])
        if(min(D[sT,tj])>cutoff){
          #print(paste('add tree',tj , min(D[sT,tj]) ))
          #print(which(D[sT,tj]==min(D[sT,tj]),arr.ind=TRUE)[[1]])
          sdist<-c(sdist,min(D[sT,tj]))
          cT<- c(cT
                 ,which(D[sT,tj]==sdist[length(sdist)], arr.ind=TRUE)[[1]] %>% sT[.] %>% list()
                 # which returns indices relative to the selected Trees in sT
          )
          sT<- c(sT,tj) # update _after_ ct has been set
        }else{ # remove node tj from the scene
          D[tj,]<- upper.bound
          #D[tj,tj:ncol(D)]<- upper.bound
          #D[tj,]<- NA
          #D[tj,tj:ncol(D)]<- NA
        }
        result$closestTree<-cT # indices relate to trees ranging 1,2,.. as ordered in rg$forest
        result$selectTrees<-sT # indices relate to trees ranging 1,2,.. as ordered in rg$forest
      } # end of tj looping through all (possibly) added trees
  } # end of else (when cutoff value is given)
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
  nT<-nrow(D) # number of trees considered in D , we will plot one less
  #print(nT)
  
  ctd<-ctdata(D,cutoff)
  sdist<-ctd$smallestDist # of all trees considered in D
  ct<-ctd$closestTree  # length(ct) = nT -1
  sT<-ctd$selectTrees
  if(length(sT)>1){
    aT <- sT[-1] # added trees, not including the first tree, now aT,cT have the same length
    # cT, aT have values in 2:nT relating to the trees
  
    col<-rep(1,nT)
    if(!is.na(cutoff)){ col[aT]<-2 }
  
    xgrid<- 2:nT
    y<- rep(NA,nT) # for all trees , even the 1st tree which we will not need later on
    for(i in seq.int(1,length(aT))){
      y[aT[[i]]]<- D[aT[[i]], ct[[i]]]
      }
  
    #print(xgrid)
    #print(y)
    print('ylim')
    print(c(0,min(max(sdist, na.rm = TRUE)+1,max(sdist, na.rm = TRUE)*1.05)))
  
    plot(x=xgrid
         , y=y[xgrid]
         , xlim=c(min(xgrid),max(xgrid))
         , xlab='added tree'
         , ylim=c(0,min(max(sdist, na.rm = TRUE)+1,max(sdist, na.rm = TRUE)*1.05))
         , ylab='smallest distance to previoulsly added trees'
         , col=col[xgrid]
         , main='added tree plot: closest tree'
         )
  
    if(!is.na(cutoff)){ abline(h=cutoff, col='grey') }
  
    labels<-c(rep('',times=nT)) # for all trees 1.. nT
    for(i in seq.int(1,length(aT))){
      labels[aT[[i]]]<- ct[[i]]
    }
    text(x=xgrid
         , y=y[xgrid]
         , pos=1
         , labels=labels[xgrid]
        )
    # labels should be lists when more than one tree is closest (will happen often for d0 metric)
  }else{message('no trees added, nothing to plot')}
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
