# create a matrix as expected for input in mds, atp, ctp
D <- matrix(round(runif(25,0,5)),ncol=5, nrow=5)
D <- D %*%t(D) # symmetric
diag(D)<-0 # 0 on diagonal
D


mds(D)
atp(D, addLabels = T)
ctdata(D,20)
ctplot(D,20)
