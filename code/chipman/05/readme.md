metrices are now scaled to be in the unit interval

* We will look at how trees are distributed over the unit square in a 2dim mds in the different metrices.
* We will check if there are natural clusters in the tree space (now the unit square) 
ie if the tree distribution over the tree space is multimodal (or not, then unimodal)

# basecase 1: randomly selecting trees
Calculate the distances of the selected trees as a matrix (dist object?), calculate the diameter of the selection (largest distance) and the mean distance (typical cluster quality measures). Is there a correlation of accuracy and any of these statistics?

# basecase 2: sub-forest of cluster medoids
Cluster the tree space, for each cluster select the medoid into the sub-forest. Medoids will naturally have larger distances than the randomly selected trees (check with data). But does it help to get a higher accuracy?

Use each individual cluster as a sub-forest. Are there clusters with higher accuracy than others. Is this consistent over the valdation and test set?