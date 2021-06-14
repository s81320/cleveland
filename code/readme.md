for discussion on the cleveland data

# up for discussion: small correlation

when selecting (hopefully) representative trees one wants the trees to be diverse / uncorrelated. And why not choose trees that perform well / above average rather than poorly?

To have a basis to decide which tree performs well we evaluate their performance on a validation set. We expect performance (accuracy) on the validation and the test set (or one might call it the second validation set) to be comparable, positively correlated, and would like to see a strong correlation.

This is not what I found. And this is probably what is referred to as 'decision trees are not robust'.

## improvement

Robustness of the random forest is an ensemble effect, the single tree is allowed to overfit. When building a ranger random forest from the ranger default parameters for unlimited tree depth and minimal node size (the natural limit here is 1) trees will overfit. Accuracies of individual trees on the validation and test set are quite uncorrelated.
Setting max.depth and min.node.size to 5 increased correlation to a mild level. (How does this affect the accuracy of the forest?)
Another approach would be to prune all trees. Since we do not work with the random forest later on, we need not care how this pruning affects the performance of the random forest. All we care about is the performance of the selected sub-forest.


