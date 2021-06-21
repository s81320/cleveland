# Speed of random forest predictions

Getting predictions for a random forest of 500 trees should take longer than getting predictions for a random forest of 50 trees. But how much longer exactly?

We should get a graph for a full forest of 500 trees and then halve the number of trees to 250 , 125, 62, 31

I use the microbenchmark package suggested in http://adv-r.had.co.nz/Profiling.html

Of course to speed up prediction time we would do predictions for the individual trees in parallel and then collect all results for the majority vote. 

## first result, sequential execution
On my laptop reducing the size of the forest from 500 to 60 (500/8=62.5, halving it 3 times) the execution time is reduced to about 40% , 45% , a tweak better than halved. So, roughly: Halving the number of trees 3 times results in halving the execution time (once).

Another calculation can be summarised as: Reducing the number of trees to 10% did more than half the execution time.

(This is ok, since 50 is about equal to 60 and reducing a forest by 10 trees should not make much difference. It is no linear relationship...)
