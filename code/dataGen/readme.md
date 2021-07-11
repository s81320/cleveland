we work with the cleveland data , split it into training, validation and test set and grow a ranger random forest on the training set...

And whatever comes next, it will also / maybe only slightly depend on how we split the data. To avoid this we create multiple data splits and grow a new forest on each training set. (We could have grown multiple forests on each training set, and we did, and we found there is little variation in accuracies for forests built on the same training set. What we could do is: work with larger forests...)

Speaking of variation: (probably) depending on the data split our (full) forests sometimes perform quite differently on the validation and test sets (cf accuracies$val.minus.test in dataGen02 , cf 02 below). And that is important for us: Whenever trying to be smart and to select a (small) subforest with a hopefully high accuracy and testing it on the test set, what good will it do if the testset is pretty close to the training data ... or not. We do not want test set qualities to reflect on our selection. One way to avoid this is to have multiple test sets. (we could have done k-fold cross validation. At the moment we only have independent data splits)

# data generation

Here we define the data we work on: the data splits, the forests. And we calculate accuracies (for the full forest on validation and test and out of bag accuracies for each tree in each forest) we will need later on (maybe repeatedly, and we don't want to repeatedly calculate them)

## 01

we do a big for loop.

filenames for results : 
* doc-500trees-10rep-5maxDepth.rda : each forest has 500 trees , there are 10 repetitions of splitting, growing, calculating accuracies, each tree has a maximum depth of 5 (to keep runtimes at bay and: to make trees a little more stable than they would otherwise be. Also dissimilarities might react to some trees growing very deep...)

* doc-500trees-10rep-5maxDepth-keepInbag.rda : as above, but now out of bag accuracies are added (based on keepInbag=T in the ranger arguments)

folder data is saved to : data

## 02

we work more sequentially, with some small loops.

folder data is saved to: data/10forests
saved objects have an info indicating the script file they were generated with, and a time of creation

## remember how to write tables and include images

tree 1 | tree 2 | tree 3
:---:|:---:|:---:
![](tree1.svg)  | ![](tree1.svg) | ![](tree1.svg)
