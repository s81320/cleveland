Looping over different data splits (into train, val, test) with a new forest grown on each new training set.

As in folder 01 now anew for each data split and each random forest:
 * create all distance matrices
 * for each distance measure collect sets of representtive trees (with additional clustering) and calculate accuracy ratios for the (clustered) subforests
 * to finally check if the subsets perform equally well or even better than the full forest (accuracy ratio ==1 or >1) 
