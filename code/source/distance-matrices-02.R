# d0, d1, d2 : based on Banerjee 2012 paper
# identifying representative trees from ensembles
# https://deepblue.lib.umich.edu/bitstream/handle/2027.42/92082/sim4492.pdf;jsessionid=1BD56DC77B9AC9DA2B39632B5459528A?sequence=1

# Shannon-Banks-metric : introduced by Shannon and Banks, 1999
# https://pubmed.ncbi.nlm.nih.gov/10204200/
# cited in Chipman, George, McCulloch , 1998, available at
# http://www-stat.wharton.upenn.edu/~edgeorge/Research_papers/ , formula (5) on page 4
# has a paremter k that should be optimized: since trees are of different height / depth 
# I cut off all trees at the same depth k . 

# when comparing 2 trees wrt their architecture
# we have to compare them as full binary trees
# The ranger forest has a shorter notation that does not allow for direct comparison
# We compare fully binary coded trees using Hamming distance


library('e1071') # needed in shannon banks metric

createDMd0 <- function(forest){
  luc<-mapply(function(a,b) unique(a[which(b[[1]]!=0)]) # get split vars for inner nodes
              , forest$split.varIDs
              , forest$child.nodeIDs
              , SIMPLIFY=F # with the default SIMPLIFY=T we may sometimes get a matrix 
              # (if all lengths are equal or even a vector if all lengths are equally equal to 1)
              # this way we always get a list. May not really be efficient , sometimes ...
              # ... anytime we could have gotten a matrix :-)
  )
  d0<-function(a,b){length(base::union(a,b)) - length(base::intersect(a,b))}
  
  nVar<-length(forest$independent.variable.names)
  
  return(outer(luc,luc,Vectorize(d0))/nVar)
}

createDMd1 <- function(forest, dft){
  nT<-forest$num.trees
  
  if (is.null(dft)){stop('data frame of training data missing. Plese, use argument dft=')}
  
  nObs<-nrow(dft)
  
  pp<-predict(forest, # predict works with a ranger object or a ranger.forest object
              data=dft, # data.frame training
              predict.all=TRUE,
              type='terminalNodes'
  )$predictions
  
  f1<-function(vec){
    outer(vec,vec,function(a,b){as.integer(a!=b)})
  } # output as matrix 
  
  A<-apply(pp,2,f1) # output format : 25x11 instead of 5x5x11
  dim(A)<-c(nObs,nObs,nT) # number of Obs x number of Obs x number of Trees

  d1<-function(i,j){sum(abs(A[,,i]-A[,,j]))} # a function
  
  return(outer(1:nT,1:nT,Vectorize(d1))) # a matrix , dim  nT , nT
  # divide by n over 2 is missing
}

createDMd2 <- function(forest, dft){
  pp<-predict(forest, # predict works with a ranger object or a ranger.forest object
              data=dft, # data.frame training
              predict.all = TRUE,
              # type='response' # it's the default
  )$predictions
  
  f1<-function(vec,vec2) sum(abs(vec-vec2))
  f2<-function(i,j) f1(pp[,i],pp[,j]) # trees i,j
  nT<-forest$num.trees
  return(outer(1:nT,1:nT,Vectorize(f2)))
}

createDMsb <- function(forest){
  rt2fbt<-function(rgf=rf$forest, trindx=1){ 
    # ranger tree to full binary tree
    # rgf : forest object of a ranger object (a random forest)
    # trindx : tree index , only 1 index, not a list of indices!
    
    t1<-rgf$child.nodeIDs[[trindx]] # a fixed tree
    ld<-t1[[1]]  # left children / daughters
    rd<-t1[[2]]  # right
    
    mf3<-function(a=0,b=1,k=0) {
      # root for ranger-tree a=0 , b will refer to a nodeID in ranger forest-object
      # root for full binary tree b=1 # each node number corresponds to an array position
      # initial distance to the root k=0
      
      #### vector z created at last split nodes (no further split nodes, next are leaves) ####
      if(ld[a+1]==0){
        if(rd[a+1]==0){
          z<-c(rep(0,2**k-1)) # for this last split node (no further splitting, next is a leaf) the tree has (at least) 2**k-1 inner nodes. Local knowledge
          #print(paste(k, ' created z of length ' , length(z)))
          # z<-c(rep(0,2**(k+1)-1)) # unnecessary to create at the level of leaves / terminal nodes, there are no split variables to document! 
          # k is distance from the root , b in 2**k:(2**(k+1)-1)
          #print(paste('create z at node',a,b))
          return(z)}
        else{
          message(paste('error in mf3 at level ',k)) # error if only one child exists
          return(-1)
        }
      }
      #### at split nodes ####
      else{ #### recursive function calls for left and right sub-trees ####
        left.st<-mf3(ld[a+1], 2*b, k+1)
        right.st<-mf3(rd[a+1], 2*b+1, k+1)
        
        #print('merging')
        #print(z1)
        #print(z2)
        # adding z1 + z2 when they may be of unequal length
        l1<-length(left.st)
        l2<-length(right.st)
        if(l1!=l2){
          # append 0 to the shorter one, until of same length
          if(l1<l2){
            left.st <-c(left.st,c(rep(0,l2-l1))) }
          else{
            right.st<-c(right.st,c(rep(0,l1-l2))) }
        }
      }
      
      z<-left.st+right.st
      
      # split vars start at 0 so we have to +1 , z is initialized with 0 which is not in split.varIDs
      z[b]<-1+rgf$split.varIDs[[trindx]][a+1]
      #print(paste(b, z[b], rgf$split.varIDs[[trindx]][a+1]))
      
      #print('merged to')
      #print(z)
      return(z)
    }
    
    return(mf3())
  }
  
  k<-5 # this should be a variable !! or make it the optimal value by adapting to the highest tree (why should that be optimal??)
  # at the moment (10.6.2021) I work with a ranger RF with max.depth=5
  A<-matrix(0,forest$num.trees,2**k-1)
  for(i in 1:forest$num.trees){
    A[i,]<-rt2fbt(forest,i)[1:2**k-1]
  }
  A[is.na(A)] <- 0 # na will occur if a full binary tree has less than 2**k-1 inner nodes
  
  return(e1071::hamming.distance(A,A)) # distance 
}

createDM <- function(forest, type, dft=NULL){
  # types d1 , d2 require the data.frame of the training set : dft
  if(type=='d0'){ createDMd0(forest) }
  else{ 
    if(type=='d1'){ 
      if(is.null(dft)){
        message('training data needed, please use argument dft=')}
      createDMd1(forest, dft) }
    else{
      if(type=='d2'){
        if(is.null(dft)){
          message('training data needed, please use argument dft=')}
        createDMd2(forest, dft) }
      else{
        if(type=='sb'){ createDMsb(forest) }
        else{ message(paste('undefined type ', as.character(type))) }
      } # end of else : when neither d0, d1 nor d2
      } # end of else : when neither d0 nor d1
    } # end of else : when not d0
  }
