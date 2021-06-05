
# constructor
subforest<-function(forest,trindx){
  sf<- forest # copy? point?
  sf$child.nodeIDs<-forest$child.nodeIDs[trindx]
  sf$split.varIDs<-forest$split.varIDs[trindx]
  sf$split.values<-forest$split.values[trindx]
  sf$num.trees<-length(trindx)
  class(sf)<-c(class(sf),'subforest') 
  return(sf)
}

# constructor
forestHull<- function(forest){
  # returns a ranger object with forest as its forest
  
  # sometimes we need a ranger object to pass as an argument though only forest is worked with
  # we create a minimal hull for a forest that is of class ranger
  
  hull<-list('forest'=list(),'num.trees'=0L)
  class(hull)<-c('ranger','forestHull')
  hull$forest<-forest
  hull$num.trees <- forest$num.trees
  return(hull)
}
