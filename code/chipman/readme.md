# Tree metrices

We use metrices as in Chipman (1998). The names d0, d1 ,d2 are as in Banerjee (2012, identifying representative trees from ensembles).

* d0, the split metric, defined as ($ d_0(T_1,T_2) = \frac{no of predictor mismatches between T_1 and T_2}{no of predictors in data set}$)
* d1, the partition metric
* d2, the fit metric (aka prediction metric)
* sb, the Shannon Banks or tree metric
which relates to the architecture of the tree, its nodes seen as a subset of nodes of a full binary tree. 
The current implementation is only for the simplest version of this metric.

Not yet implemented is the d1* metric (cf Banerjee) which is a product of the split and the partition metric.

Tree metrices are implemented in distance-matrices-02.R https://github.com/s81320/cleveland/blob/f8621f3d806a1240d04c24d6d59795df9c63710b/code/source/distance-matrices-02.R  and distance-matrices-03-scaled01.R
