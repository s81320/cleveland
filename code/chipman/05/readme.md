# what is new in this version 05
metrices are now scaled to be in the unit interval

* We now look at how trees are distributed over the unit square in a 2dim mds in the different metrices.

## what we plan to do
We will check if there are natural clusters in the tree space (now the unit square) 
ie if the tree distribution over the tree space is multimodal (or not, then unimodal)

# Chipman results
Building a subforest from the added tree plot with cutoff set to the mean distance of trees (the cutoff value is updated for every metric and every forest / data split with a new forest grown on the current training data). The table shows accuracy ratios and average sizes of the selected sub-forest for each metric (so there is no easy comparison to the base cases)

metric | mean accuracy ratio | mean size of sub-forest
--- | --- | ---
d0 | 99.6 | 12
d1 | 1 | 39
d2 | 1.01 | 33
sb | 98.8 | 17.3

When lowering the cutoff to 0.7 times the mean distance we get larger sub-forests (size about 200) that will then be clustered and can be compared to the base cases. We should expect nothing different from the 2nd base case: applying cutoff first and then clustering should not make clustering easier. Actually, first destroying similarities and then clustering by similarity should leave the algorithm clueless?! How to cluster similar trees together if you first made sure that trees are not similar. Well... anyways:

3 | 5 | 7 | 11 | 13
---|---|---|---|---
94.2 | 97.3 | 97.3 | 98.7 | 97.9

Standard deviation is around 5 %.

and it seems we reduce our performance by clustering.