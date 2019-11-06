NEWS
================
Shuowen Chen
11/4/2019

# SortedEffects 1.1.0.
This note document the changes from SortedEffects 1.0.0. The following changes are suggested by Norman Martloff, the R Journal editor, Thomas Leeper and an anonymous referee from the R Journal. 

## Major Changes
1. Compared to the previous version 1.0.0, the package now has a S3 structure. The plotting functions (`plot.spe`, `plot.ca` and `plot.subpop`) now are methods of generic `plot()`. 

2. In addition to the plotting methods, we provide three new summary methods (`summary.spe`, `summary.ca` and `summary.subpop`), which are methods of generic `summary()`. We believe these three new methods provide richer outputs to the users. 

## Minor Changes
1. We decaptalize the three functions (`spe`, `ca`, `subpop`). 
2. We rename some arguments in the three functions to be snake case rather than dot case. These are `var` (instead of `var.T`), `boot_type` (instead of `boot.type`), `range_cb` (instead of `range.cb`), `var_type` (instead of `var.type`). We rename the argument `B` in the three functions to be `b`, and change the default to 500. 
3. The arguments `method`, `boot_type`, `var_type` are have a finite set of options for users to choose from. So we now explicitly provide these options in the argument, and the code will use `match.arg()` to match user input for execution. 
4. In each of the three functions (`spe`, `ca`, `subpop`) we add an option called `parallel` that allows users to either turn on or off the parallel computing. If users turn on the parallel computing, they can further specify how many CPUs to use via the `cores` option. 
5. The package now has a progress bar showing the progress of bootstrap estimation. It also shows how many CPUs the users are using. 
6. The option `cl` in function `ca` wasn't very intuitive. Now the users can choose from two alternatives (`diff` or `hoth`), which are the most common for `cl`. 
7. In `plot.subpop`, we add an option called `overlap` that allow users to either keep or drop the overlapped observations in the plot. 
8. In the previous version, the option `t` and `cat` in function `ca` required users to manually input all the variables. In particular, if some variables of interest are factor, the users need to manually create a lot of indicator variables and type them as an input. In the current version, users only need to input the factor variable itself, and the code will automatically generate indicator variables and run the estiamtions. We believe this makes the package easier to use. 
9. In each function (`spe`, `ca`, `subpop`), we change the default of option `taus` to `c(5:95)/100`. 
10 In function `ca`, we change the default to `c(1:99)/100`. 
