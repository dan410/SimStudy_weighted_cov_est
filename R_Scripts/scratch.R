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
nsim = 6
sim_specs <- data.frame(dep = rep(0.2, nsim), grid_ID = rep(3, nsim), weight = rep(1:6, each=nsim/6))
sim_specs$sim_ID <- 1:nrow(sim_specs)

# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)


# sim_disk_conn <- localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/sim_kv",
# 															autoYes = TRUE,
# 															reset = TRUE)

sim_disk_conn <- localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv",
															autoYes = TRUE,
															reset = TRUE)

addData(sim_disk_conn, bySimID, overwrite = TRUE)

############################################################################
### 
############################################################################

### using mclapply
# define a function which will be called with the arguments corresponding to each row
nsim = 6*10
sim_specs <- data.frame(dep = rep(0.2, nsim), grid_ID = rep(3, nsim), weight = rep(1:6, each=nsim/6))
sim_specs$sim_ID <- 1:nrow(sim_specs)

mylist <- as.list(as.data.frame(t(sim_specs)))

covf_stat <- function(x){
res <- spatial_calc(dep = x[1], grid_ID = x[2], weight = x[3], sim_locs = sim_locs)
}


time.mclapply <- system.time(
	test <- mclapply(mylist, FUN = covf_stat, mc.cores = 4)
)





junk <- as.list(as.data.frame(t(sim_specs)))

############################################################################
### 
############################################################################

#### using plyr ####

res <- lapply(bySimID, .fun = )

library(parallel)

test <- lapply(1:100,function(x) rnorm(10000))
system.time(x <- lapply(test,function(x) loess.smooth(x,x)))
system.time(x <- mclapply(test,function(x) loess.smooth(x,x), mc.cores=4))
