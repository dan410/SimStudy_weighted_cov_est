library(datadr)

weights <- round(c(0, 1/2, 1, 2, 3, 4), 2)
nsim = length(weights)*30

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




