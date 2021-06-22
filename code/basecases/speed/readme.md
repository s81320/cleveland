# Speed of random forest predictions

Getting predictions for a random forest of 500 trees should take longer than getting predictions for a random forest of 50 trees. But how much longer exactly?

I use the microbenchmark package suggested in http://adv-r.had.co.nz/Profiling.html

Of course to speed up prediction time we would do predictions for the individual trees in parallel and then collect all results for the majority vote. This is available in ranger when setting num.threads in ranger::predict. The effect of reducing the number of trees in each thread should be similar (though not the same) to the effect when working in a sequential mode only (only the individual predictions are parallized, the vote has to run on the combined predictions).

## first result, sequential execution
Execution time is (affine) linear in the number of trees, cf .png
