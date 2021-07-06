# base case 2 : cluster medoids

From a random forest, build a clustering of its trees. From each cluster choose the mediod to built the subforest.

The difference from random sampling / base case 1 is that trees have a high distance from each other. This is one of the main aspects of clustering: trees clusterd toghether are close, trees clustered into different clusters are far. While this need not always hold and trees on the edge of a cluster may be quite close to trees from another cluster, it does hold for trees at the center of a cluster: Medoids of (different) clusters are far from each other.

## results

we report cluster sizes and mean accuracy ratios (*accuracy ratio* : ratio / quotient of the full forest's accuracy and the accuracy of the sub-forest , *mean* over the different metrices and 10 repetitions of new data splits and random forests grown anew on each training set)

3 | 5 | 7 | 11 | 13
---| --- | ---| --- | ---
96.3 | 97.9 | 96.5 | 98.6 | 98.8

This is similar to the random sampling. Also 10 repetitions do not seem enough to test for difference in the mean when their difference is so small...

Looking into the accuracy ratios for the different metrices we find that d1 often performs worse than its peers, so we remove it to get a result slightly better than random sampling.

3 | 5 | 7 | 11 | 13
---| --- | ---| --- | ---
97.0 | 98.5 | 97.2 | 98.7 | 99.5

This might indicate that building a sub-forest from trees that are far apart from each other positively affects the accuracy ratio.

We look into the performance for the largest number of clusters , k= 13.

On the validation set we have the following mean accuracy ratios

metric | mean acc ratio | sd acc ratio
---|---| ---
d0 | 0.997 | 0.04
d1 | 0.963 | 0.04
d2 | 0.997 | 0.04
sb | 0.985 | 0.07

and on the test set

metric | mean acc ratio | sd acc ratio
---|---| ---
d0 | 1.08 | 0.1
d1 | 1.1 | 0.12
d2 | 1.11 | 0.11
sb | 1.08 | 0.08