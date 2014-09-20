############################################################################
### Scratch Work
############################################################################
library(dplyr)

sim_locs <- readRDS("Data/sim_locs.rds")


nsim <- 100;
junk <- list();
system.time(
	for( i in 1:nsim){
		junk[[i]] <- spatial_calc(dep = 0.3, grid_ID = 3, weight = 3, sim_locs = sim_locs)
	}
)
df <- do.call(rbind, junk)

### using datadr 
nsim = 6*10
sim_specs <- data.frame(dep = rep(0.2, nsim), grid_ID = rep(3, nsim), weight = rep(1:6, each=nsim/6))
sim_specs$sim_ID <- 1:nrow(sim_specs)

# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)


sim_disk_conn <- localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/sim_kv", 
															autoYes = TRUE, 
															reset = TRUE)

addData(sim_disk_conn, bySimID, overwrite = TRUE)



#### using plyr ####

res <- lapply(bySimID, .fun = )


