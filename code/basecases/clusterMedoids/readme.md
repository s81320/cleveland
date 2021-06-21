# base case 2 : cluster medoids

From a random forest, build a clustering of its trees. From each cluster choose the mediod to built the subforest.

The difference from random sampling / base case 1 is that trees have a high distance from each other. This is one of the main aspects of clustering: trees clusterd toghether are close, trees clustered into different clusters are far. While this need not always hold and trees on the edge of a cluster may be quite close to trees from another cluster, it does hod for trees at the center of a cluster: Medoids of (different) cluster are far from each other.
