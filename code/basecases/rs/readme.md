# base case 1
Randomly sample trees from the forest and build a sub-forest from these trees.

This is the same as starting out with small sizes of a random forest. In ranger: set num.trees to the number of trees that you want in the sub-forest.

# results (01)
For small sizes of a subforest: 3,5,7,11,13 we get accuracy ratios (in per cent, \%) approaching 1
3 | 5 | 7 |11 | 13
--- | --- | --- | --- | ---
94.49 | 96.82 | 97.46 | 98.8 | 98.99

Standard deviations of these accuracy ratios go down but are ranging from 7.5 \% to 5.3, \% with some outliers.

While a random forest with 50 trees may increase very little in accuracy when adding more trees it does reduce standard deviation. In reverse: When randomly sampling trees from a forest to build a sub-forest we may get accuracy ratios close to one. But we are under a higher risk of unluckily sampling a sub-forest with accuracy quite a bit lower than the full forest / an accuracy ratio quite far from 1.

## with additional info (02)

We randomly sample trees into a sub-forest and document their 
* mean distance (currently only in the d0 metric / dissimilarity measure) and
* their mean oob accuracy (the mean of each trees individual oob accuracy, based on training data that did not go into building the tree).

We apply a linear model and find that both , the mean distance and the mean oob accuracy significantly (and positively) affect the accuracy ratio on the validation set.

# further steps

To generate a sub-forest of distant trees we cluster the forest and select its medoids. By construction the medoids are far from each other, their mean distance will be large.
