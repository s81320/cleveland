# Speeding up Random Forest Predictions

What can you do to speed up predictions made by a random forest?

Predictions have 2 parts: Individual: individual predictions of the trees of the forest. Ensemble: constructing an ensemble vote from the individual votes. Often done through majority vote.

Proposition: in the 1st part, we calculate individual predictions only for a subset of trees. And in the 2nd part we get the majority vote for the subset of trees. This should speed things up.

We compare a forest of 500 trees with its subforests of 250, 200, ... , 50 , 40, 30 , 20 ,10 trees. Using the same random forest prediction function (from the ranger packge). We hope to get nice linegraph with a descending graph.

Maybe we could do this for a forest of 5000 as well or for other data sets.

## What about accuracy?

Speed is one thing, accuracy another. We should look into a basecase of randomly selected trees and look at their accuracy. What is the general trade-off between speed and accuracy?

### Smart selection of trees: representative trees?