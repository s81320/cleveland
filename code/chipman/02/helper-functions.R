# helper functions: 
## accuracy
## accuracy for the predictions of a subforest (a p sf)
## checkPartition

acc <- function (vec1, vec2=df[test,'CAD']) {
  if(length(vec1)==length(vec2))
  {return(length(which(vec1==vec2))/length(vec1))}
  else {return(-1)}
}

apsf<- function(forest=rg$forest, idx, newdata){
  # accuracy for the predictions of a subforest
  idx %>%
    subforest(forest,.) %>%
    predict(data=newdata) %>%
    .$predictions %>%
    acc(newdata$CAD)
}


checkPartition<-function(train_=train, val_=val, test_=test, df_=df){ 
  
  OK<-FALSE
  
  diff.in.strat <- (as.vector(prop.table(table(df_[train_,]$CAD))) +
                      as.vector(prop.table(table(df_[val_,]$CAD))) +
                      as.vector(prop.table(table(df_[test_,]$CAD))) - 
                      3*as.vector(prop.table(table(df_$CAD)))) %>%
    abs() %>%
    max()
  
  if(
    # check that the sizes of train, val and test set add to the size of the data set (number of rows)
    length(union(union(test_,val_),train_))==nrow(df_) &
    
    # check disjointness of train, val, test sets
    length(base::intersect(test_,val_))+length(base::intersect(train_,val_))+length(base::intersect(train_,test_))==0 & 
    
    # rough check that the class balance is about equal in all parts and in the overall data set , probably too rough ...
    diff.in.strat<0.05) { OK<-TRUE }
  
  return(OK)
}

# accuracy
acc.test<- function(){
print(acc(c(1,2,3,4),c(1,2,3,1)) == 3/4) # 3/4
print(acc(c(1,2,3,4))==-1) # -1 
}

checkPartition.test <- function(){
# checkPartition() # requires values for the defaults... and should return TRUE
# checkPartition(c(1,2,3)) # should return FALSE
# the following should return TRUE , any messing with the arguments should return FALSE
  checkPartition( c(1,2,3), c(4,5), c(6,7), as.data.frame(matrix(0,nrow=7,ncol=2)) )
}

