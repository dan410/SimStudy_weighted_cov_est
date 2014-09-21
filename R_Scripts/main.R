library(sseigfun) # my package for covariance estimation
library(sfdasim) # my package for simulating spatially distributed curves
library(geoR) # used to simulate gaussian random fields
library(spatstat) # simulating point processes
library(datadr) # map-reduce package

### ON PIC ### setwd("~/Dissertation_projects/SimStudy_weighted_cov_est/")
### Local ### setwd("/Users/fort002/Google Drive/Research/Projects/SimStudy_weighted_cov_est")

# source R functions
path = "R"
for (nm in list.files(path, pattern = "[.][Rr]$")) {
       source(file.path(path, nm))
}

# read in set of location configurations to use 
sim_locs <- readRDS("Data/sim_locs.rds")

### using datadr 
nsim = 6
sim_specs <- data.frame(dep = rep(0.2, nsim), grid_ID = rep(3, nsim), weight = rep(1:6, each=nsim/6))
sim_specs$sim_ID <- 1:nrow(sim_specs)

# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)

