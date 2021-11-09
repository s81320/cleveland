# there should be a test file ??
# for the oob accuracy ... and other stuff...

outer(1:3,1:4, function(i,j) i+j) # works fine
outer(1:2,1:2,myAcc)
outer(1:2,1:2,Vectorize(myAcc)) # as expected
myAcc(1,1) ; myAcc(1,2) ; myAcc(2,1) ; myAcc(2,2)

outer(1:3,1:3,Vectorize(myAcc)) # as expected
myAcc(1,3) ; myAcc(2,3) ; myAcc(3,3)
myAcc(3,1) ; myAcc(3,2)

outer(1:3,1:7,Vectorize(myAcc)) # as expected
mean(outer(1:2,1:N,Vectorize(myAcc))) # as expected

View(myAcc)


