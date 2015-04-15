setwd('/Users/fort002/Google Drive/Research/Projects/SimStudy_weighted_cov_est')

# create data lists
library(sfdasim) # my package for simulating spatially distributed curves


###
grid_ID <- 3
sigma <- 0.01
dep <- 1
obs_per_curve <- 20
sim_locs <- readRDS("Data/sim_locs.rds")


DAT <- list()

for(i in 1:200){ 
  locs <- subset(sim_locs, grid == grid_ID)[, c("x","y")]
  intensity <- subset(sim_locs, grid == grid_ID)$intensity
  
  curves <- sim_sfda_curves(nBasis = 3, 
                            type = "Cos", 
                            basis.pars = 2,
                            cov.model = rep("exponential", 3), 
                            cov.pars = rbind(c(1, dep), c(1, dep), c(1, dep)),
                            locs = locs)
  
  ### generate observed data 
  m <- obs_per_curve  # number of observations per curve
  times <- runif(m, min = 0.01, max = 0.99)
  dat <- with(curves, sim_sfda_data(locs = locs, coef = coef, basis.fns = basis.fns, sigma = sigma, pts = times))
  
  DAT[[i]] <- dat
}

saveRDS(DAT, file = paste("Data/dat_grid_", grid_ID, "_sig_", sigma, "_dep_", dep, ".rds", sep=""))


### Grid data
d1 <- readRDS("Data/dat_grid_3_sig_0.01_dep_0.rds")
d2 <- readRDS("Data/dat_grid_3_sig_0.01_dep_0.1.rds")
d3 <- readRDS("Data/dat_grid_3_sig_0.01_dep_0.2.rds")
d4 <- readRDS("Data/dat_grid_3_sig_0.01_dep_0.3.rds")
d5 <- readRDS("Data/dat_grid_3_sig_0.01_dep_0.4.rds")

d6 <- readRDS("Data/dat_grid_3_sig_0.3_dep_0.rds")
d7 <- readRDS("Data/dat_grid_3_sig_0.3_dep_0.1.rds")
d8 <- readRDS("Data/dat_grid_3_sig_0.3_dep_0.2.rds")
d9 <- readRDS("Data/dat_grid_3_sig_0.3_dep_0.3.rds")
d10 <- readRDS("Data/dat_grid_3_sig_0.3_dep_0.4.rds")

GRID <- c(d1, d2, d3, d4, d5)
saveRDS(GRID, "Data/GRID3.rds")









