# Speed of random forest predictions

Getting predictions for a random forest of 500 trees should take longer than getting predictions for a random forest of 50 trees. But how much longer exactly?

We should get a graph for a full forest of 500 trees and then reduce its size to 80%, 55%, 50%, 45%, 30%, 25%, 20%, 15%, 12.5%, 10%, 5%, 1%

And get a number of what reduction in time we get from halving the number of trees once (50%), twice (25%), and thrice (12.5%).

Maybe we could get the time for just 1 and 2 trees. These are upper bounds for the prediction with a single tree and the trivial majority vote with one tree...

And I should get some literature on how to measure runtimes at all...