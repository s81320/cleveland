# base case 1
Randomly sample trees from the forest and build a sub-forest from these trees.

This is the same as starting out with small sizes of a random forest. In ranger: set num.trees to the number of trees that you want in the sub-forest.

# results
For small sizes of a subforest: 3,5,7,11,13 we get accuracy ratios (in per cent, \%) approaching 1
3 | 5 | 7 |11 | 13
--- | --- | --- | --- | ---
94.49 | 96.82 | 97.46 | 98.8 | 98.99

Standard deviations of these accuracy ratios go down but are ranging from 0.075 to 0.053, no \% with some outliers.

While a random forest with 50 trees may increase very little in accuracy when adding more trees it does reduce standard deviation. In reverse: When randomly sampling trees from a forest to build a sub-forest we may get accuracy ratios close to one. But we are under a higher risk of unluckily sampling a sub-forest with accuracy quite a bit lower than the full forest / an accuracy ratio quite far from 1.