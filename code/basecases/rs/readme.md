# base case 1
Randomly sample trees from the forest and build a sub-forest from these trees.

This is the same as starting out with small sizes of a random forest. In ranger: set num.trees to the number of trees that you want in the sub-forest.

# results (01)
For small sizes of a subforest: 3,5,7,11,13,17 and 10 independent random samples for each forest and subforest size we get accuracy ratios (in per cent, \%) approaching 1
3 | 5 | 7 |11 | 13 | 17
--- | --- | --- | --- | --- | --
94.7 | 97.4 | 98.0 | 99.0 | 99.2 | 99.7

Having dome 10 independent repetitions we have some variation to be expected. These are results for 100 independent random samples for each forest and subforest size.

size | 3 | 5 | 7 |11 | 13 | 17
--- | --- | --- | --- | --- | --- | --
mean acc ratio|94.6 | 96.8 | 97.7 | 89.7 | 99.1 | 99.5
standard dev | 7.5 | 6.8 | 6.1 | 5.4 | 5.3 | 5.2

We see the mean accuracy ratio going up and standard deviations going down. 

While a random forest with 50 trees may increase very little in accuracy when adding more trees it does reduce standard deviation. In reverse: When randomly sampling trees from a forest to build a sub-forest we may get accuracy ratios close to one. But we are under a higher risk of unluckily sampling a sub-forest with accuracy quite a bit lower than the full forest / an accuracy ratio quite far from 1.

## with additional info (02)

We randomly sample trees into a sub-forest and document their 
* mean distance (currently only in the d1 metric / dissimilarity measure) and
* their mean oob accuracy (the mean of each trees individual oob accuracy, based on training data that did not go into building the tree).

We apply a linear model and find that both , the mean distance and the mean oob accuracy significantly (and positively) affect the accuracy ratio on the validation set.

Currently (9th July 2021) it looks like a dissimilarity is not relevant but a good oob accuracy might have a (small) positive influence on the accuracy ratio. (note: doing more repetitions (nRS=100) also the oob accuracy becomes significant.)

# further steps

To generate a sub-forest of distant trees we cluster the forest and select its medoids. By construction the medoids are more evently distributed over the available tree space. They cannot be close ... but on the other hand they cannot all be far from each other (each tree at the edge of the tree space). While this need not influence the mean dissimilarity it should reduce its standard deviation. If dissimilarity does not matter then this is just a different kind of random selection. A main aim is to increase accuracy but even at stalling accuracies, reducing the standard deviation (of the accuracy rate) would be appreciated...
