
library(sseigfun) # my package for covariance estimation
library(sfdasim) # my package for simulating spatially distributed curves
library(geoR) # used to simulate gaussian random fields
library(spatstat) # simulating point processes

# source R functions
path = "R"
for (nm in list.files(path, pattern = "[.][Rr]$")) {
       source(file.path(path, nm))
}


sim_locs <- readRDS("Data/sim_locs.rds")


nsim <- 20
junk <- list()

for( i in 1:nsim){
junk[[i]] <- spatial_calc(dep = 0.3, grid_ID = 1, weight = 3, sim_locs = sim_locs)
}

df <- do.call(rbind, junk)

### create k/v pairs with pars as key and empty value and run map-reduce calling the function

