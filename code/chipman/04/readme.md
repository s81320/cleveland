# Chipman, George, McCulloch 1998

Extracting Representative Tree Models from a Forest: http://www-stat.wharton.upenn.edu/~edgeorge/Research_papers/forestCART.pdf

The paper is more interested in interpretation than in performance. Interpretation is hard to do with a forest or an ensemble of trees. 
But if one can extract trees that represent the forest well then by interpreting the extracted trees one can claim an interpretation of the forest.

What if we could extract trees and build a small sub-forest with a similar performance (accuracy) as the full forest? In production we could make faster classifications.

# speeding up random forest predictions 2021

1) base case: We randomly sample trees from a forest, form a small sub-forest and check its accuracy. For easier comparison of different (full) forests with different accuracy and their sub-forests we look at the accuracy ratio: the quotient of the full forest's accuracy and the sub-forest's accuracy. (cf dataEval-baseCase-randomSampling.R)

2) base case: Cluster the (full) forest, extract each clusters medoid (which is a tree) and build a sub-forest. (cf dataEval-justCluster.R)

3) chipman approach: spanning sub-forest (cf spanning tree for a network), sub-forest of diverse trees

The chipman paper works with some / different dissimilarity measures for trees. We look at metrices d0, d1, d2 and the shannon banks metric (cf Banerjee 2012 https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.4492), for each metric we select a sub-forest by starting with an arbitrary tree and adding further trees that are of a certain minimal distance (larger than a specified cutoff value) to all trees already in the sub-forest. The size of the final sub-forest depends on the cutoff value. Chipman then clusters the diverse trees and selects the medoids as representations. (cf dataEval-chipman.R)

All three data evaluation R scripts work on the same data, the Cleveland data set. Also they all work on ten different splits of this data into training, validation and test with ten different random forests (ranger) grown on the training sets and for each forest 4 distance matrices (a total of 40 distance matrices). This data was generated in dataGeneration-split-ranger-dm.R and saved as an rda-file. This file is loaded at the beginning of each of the three dataEvaluation scripts above.

## first results

The chipman approach without clustering performed best. We obtained accuracy ratios with median 1. This was better than random sampling and simple clustering. 

## next steps

1) For speeding up predictions we need to substantially reduce the size of the full forest and select a small sub-forest. The size should be 1% or 5% , maybe as a maximum 10% of the full forest's size.

Chipmans spanning sub-forest performes well. However, the sizes of the sub-forests are sometimes too big. It would be nice to find an inverse and start with a desired size for the sub-forest and find a cutoff inducing this sub-forest size. Then it would be feasible to generate unclustred sub-forests of specified size and directly compare them to the base cases.

2) other metrices: 
* The chipman paper has a combination of two of its metrices. I have not implemented that. 

* The d0 metric compares the split variables used in different trees. 2 trees will be more similar if they share the same split variables and will even have 0 dissimilarity if they use the exact same split variables (and they will most likely not be the same). Here we might add information on when / where (in the tree, at which height) the split variables differ. Using different split variables at the root will make 2 trees more dissimilar than using different split variables closer to the leaves. Another idea would be to exploit variable importance. 2 trees that use the same split variables as important variables should be rther close. Differing in unimportant variables should have less of an effect in dissimilarity.
