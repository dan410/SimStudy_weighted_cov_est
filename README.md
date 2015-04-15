Simulation study using spatially derived weights for estimating the covariance function
=========================


This simulation study provides insight into how the covariance function estimator behaves when curves are spatially dependent. The main idea is that under spatial dependence, curves that are near each other will exhibit some amount of redundancy. By down-weighting spatially clustered curves, we may be able to more appropriately include information about the contribution of each curve to the underlying process. That is, curves that are located in high point-intensity areas should not be included in the estimation as if they were statistically independent. 

We propose a weighting scheme that is derived from the spatial point intensity (number of locations per unit area). Let
```
weight_i = f(lambda_i)
```
where the function `f()` we consider is a power function: `f(x) = (1/x)^p`. For example, when `p = 1/2` the weight for `curve_i` is equal to the square root of the inverse point intensity at `location_i`.

Note: this approach is about relative weighting. Locations on a regular grid (i.e. constant point intensity) will receive equal weighting. This approach will only be beneficial under situation of non-constant point intensity. 
 
File Organization
--------------------

### R_Scripts

This folder contains all of the R scripts for running the simulation study.

| file | Description|
|------|------------|
| `create_sim_args.R` |  create data frame of arguments to be used in simulation. Each set of unique arguments is saved as a k/v pair so that the individual calculations in the sim study can be fully parallelized  |
| `mr_run_sim.R` | map-reduce code for running the simulations. Uses datadr package with k/v inputs equal to simulation function arguments |
| `create_grid.R` | Create point location configurations for simulation study |
| `munge.R` | read in data and combine to create data frames for analysis and plotting |
| `weighted-cov-est.R` | This is a non-essential script that looks at the different covariance structures |


### Plots
Plots are generated with the script `Plots/weighted_cov_est_plots.R` and saved in the `Plots` folder.


### Data

This folder contains all the R data objects used in the project

| file | Description |
|------|-------------|
| `wtsims.RData` | contains output from simulations | 
| `sim_locs.rds` | data frame of location configurations with associated intensity values for each location  |

### TO DO
* update intensity values in the `creat_grid.R` script
