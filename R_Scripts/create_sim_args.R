library(datadr)

weights <- round(c(0, 1/3, 1/2, 1, 2, 3), 2)
nsim = length(weights)*30
sim_specs <- data.frame(dep = rep(.2, nsim), grid_ID = rep(3, nsim), weight = rep(weights, nsim/6), obs_per_curve  = 20)
sim_specs$sim_ID <- 1:nrow(sim_specs)


# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)


### Laptop Connection ####
sim_disk_conn <- localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/sim_kv",
															autoYes = TRUE)

# ### Local PIC Connection ###
# sim_disk_conn <- localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv",
# 															autoYes = TRUE,
# 															reset = TRUE)

addData(sim_disk_conn, bySimID, overwrite = TRUE)