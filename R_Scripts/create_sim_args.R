library(datadr)


############################################################################
### 
############################################################################
grid = 3
obs_per_curve = 20
deps <- c(0.001, 0.1, 0.2, 0.3)
weights <- round(c(0, 1/3, 1/2, 1, 2, 3, 4), 2)
nreps <- 100

# create a data frame with rows equal to unique factor combinations
specs <- data.frame(dep = rep(deps, each = length(weights)), weight = rep(weights, length(deps)))
specs$grid_ID <- grid
specs$obs_per_curve <- obs_per_curve

# rbind specs data frame with itself to get number of repititions
sim_specs <- NULL
for(i in 1:nreps){
	sim_specs <- rbind(sim_specs, specs)
}

# create an ID columns that will be used to create k/v pairs
sim_specs$sim_ID <- 1:nrow(sim_specs)

# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)

# ### Local PIC Connection ###
sim_disk_conn <- localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv_grid3_all",
															autoYes = TRUE)

addData(sim_disk_conn, bySimID, overwrite = TRUE)

############################################################################
### dep = 0.3, grid = 3, n = 20
############################################################################

sim_specs <- data.frame(dep = rep(.3, nsim), grid_ID = rep(3, nsim), weight = rep(weights, nsim/6), obs_per_curve  = 20)
sim_specs$sim_ID <- 1:nrow(sim_specs)


# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)


### Laptop Connection ####
sim_disk_conn <- localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/sim_kv_grid3_dep3",
															autoYes = TRUE)

# ### Local PIC Connection ###
sim_disk_conn <- localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv_grid3_dep3",
															autoYes = TRUE)

addData(sim_disk_conn, bySimID, overwrite = TRUE)

############################################################################
### dep = 0.1, grid = 3, n = 20
############################################################################

sim_specs <- data.frame(dep = rep(0.1, nsim), grid_ID = rep(3, nsim), weight = rep(weights, nsim/6), obs_per_curve  = 20)
sim_specs$sim_ID <- 1:nrow(sim_specs)


# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)


### Laptop Connection ####
sim_disk_conn <- localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/sim_kv_grid3_dep1",
															autoYes = TRUE)

# ### Local PIC Connection ###
sim_disk_conn <- localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv_grid3_dep1",
															autoYes = TRUE)

addData(sim_disk_conn, bySimID, overwrite = TRUE)

############################################################################
### dep = 0.001 "independent", grid = 3, n = 20 
############################################################################

sim_specs <- data.frame(dep = rep(.001, nsim), grid_ID = rep(3, nsim), weight = rep(weights, nsim/6), obs_per_curve  = 20)
sim_specs$sim_ID <- 1:nrow(sim_specs)


# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)


### Laptop Connection ####
sim_disk_conn <- localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/sim_kv_grid3_ind",
															autoYes = TRUE)

# ### Local PIC Connection ###
sim_disk_conn <- localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv_grid3_ind",
															autoYes = TRUE)

addData(sim_disk_conn, bySimID, overwrite = TRUE)






