library(datadr) # map-reduce package

### ON PIC ### 
setwd("~/Dissertation_projects/SimStudy_weighted_cov_est/")
### Local ### 
# setwd("/Users/fort002/Google Drive/Research/Projects/SimStudy_weighted_cov_est")


############################################################################
### Setup
############################################################################

sim_setup <- expression({
	
	library(sseigfun) # my package for covariance estimation
	library(sfdasim) # my package for simulating spatially distributed curves
	library(geoR) # used to simulate gaussian random fields
	
	### True covariance function for the process
	cov.true <- function(x,y){
		alpha = 2
		k <- 1:3
		res <- sum(k^(-2*alpha)*cos(k*pi*x)*cos(k*pi*y))
	}
	
	### 
	spatial_calc <- function(dep, grid_ID, weight, obs_per_curve, sim_locs){
	
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
		dat <- with(curves, sim_sfda_data(locs = locs, coef = coef, basis.fns = basis.fns, sigma = 0.01, pts = times))
  
		# assign weights to each point based on the intensity values
		for( j in 1:length(unique(dat$ID))){
			dat$wt[dat$ID == j] <- (1/intensity[j])^(weight)
		}
	
		########################################################
		# estimate the covariance function using weights
		########################################################
	
		# if estimation fails with an error return NA
		result = tryCatch({
		    cov.est <- estimate_cov_function(dat, n.marginal.knots = 5)
		}, warning = function(w) {
		}, error = function(e) {
			return(data.frame(dep = dep, grid_ID = grid_ID, weight = weight, L2 = NA))
		}, finally = {
		})
		
		### estimate the L2 distance between estimated cov fun and true cov fun
		tt <- seq(0,1, length =  20)
		grid <- expand.grid(t1 = tt, t2 = tt)
		cov.true.pts <- mapply(cov.true, x = grid[,1], y=grid[,2])
		cov.est.pts <- mapply(sseigfun:::cov.fn, x = grid[,1], y=grid[,2], MoreArgs = list(knots=cov.est$knots, fit.obj=cov.est))
		dist.L2 <- sum((cov.true.pts - cov.est.pts)^2)/nrow(grid)
	
		res <- data.frame(dep = dep, grid_ID = grid_ID, weight = weight, obs_per_curve = obs_per_curve, L2 = dist.L2)
		return(res)

	}
	
})

############################################################################
### map
############################################################################

sim_map <- expression({
   for(i in 1:length(map.values)){
		 x <- map.values[[i]]
		 res <- spatial_calc(dep = x$dep, grid_ID = x$grid, weight = x$weight, obs_per_curve = x$obs_per_curve, sim_locs = sim_locs) 
		 collect("result", res)
	 }
})

############################################################################
### Reduce
############################################################################

# reduce collects map results and then iteratively rbinds them and returns top 5
sim_reduce <- expression(
   pre = {
		 count <- 1
		 res_list <- list()
   }, reduce = {
      res_list[[count]] <- do.call(rbind, reduce.values)
			count <- count + 1
   }, post = {
		 res <- do.call(rbind, res_list)
     collect(reduce.key, res)
   }
)

############################################################################
### Read in data needed in map-reduce
############################################################################


## read in object with different configuration of spatial locations
sim_locs <- readRDS("Data/sim_locs.rds")


############################################################################
### Execute on PIC
############################################################################

#### grid 3, dep 0.1
## Create a ddf object with values equal to simulatin parameters
bySimID <- ddf(localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv_grid3_dep1"), update = TRUE)

# execute the job
timing <- system.time(
sim_result <- mrExec(bySimID, 
										params = list(sim_locs = sim_locs),
										setup = sim_setup, 
										map = sim_map, 
										reduce = sim_reduce,
										output = localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_res_grid3_dep1", autoYes= TRUE), 
										overwrite = TRUE)
)
timing

#### grid 3, dep 0.3
## Create a ddf object with values equal to simulatin parameters
bySimID <- ddf(localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv_grid3_dep3"), update = TRUE)

# execute the job
timing <- system.time(
sim_result <- mrExec(bySimID, 
										params = list(sim_locs = sim_locs),
										setup = sim_setup, 
										map = sim_map, 
										reduce = sim_reduce,
										output = localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_res_grid3_dep3", autoYes= TRUE), 
										overwrite = TRUE)
)
timing

#### grid 3, independent
## Create a ddf object with values equal to simulatin parameters
bySimID <- ddf(localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv_grid3_ind"), update = TRUE)

# execute the job
timing <- system.time(
sim_result <- mrExec(bySimID, 
										params = list(sim_locs = sim_locs),
										setup = sim_setup, 
										map = sim_map, 
										reduce = sim_reduce,
										output = localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_res_grid3_ind", autoYes= TRUE), 
										overwrite = TRUE)
)
timing

#### grid 3, all
## Create a ddf object with values equal to simulatin parameters
bySimID <- ddf(localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_kv_grid3_all"), update = TRUE)

# execute the job
timing <- system.time(
sim_result <- mrExec(bySimID, 
										params = list(sim_locs = sim_locs),
										setup = sim_setup, 
										map = sim_map, 
										reduce = sim_reduce,
										output = localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_res_grid3_all", autoYes= TRUE), 
										overwrite = TRUE)
)
timing



############################################################################
### Execute Local
############################################################################


# ## Create a ddf object with values equal to simulatin parameters
# bySimID <- ddf(localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/sim_kv"), update = TRUE)
#
# # create a multicore cluster
# library(parallel)
# cl <- makeCluster(4)
#
# # execute the job
# timing <- system.time(
# sim_result <- mrExec(bySimID,
# 										params = list(sim_locs = sim_locs),
# 										setup = sim_setup,
# 										map = sim_map,
# 										reduce = sim_reduce,
# 										control = localDiskControl(cluster = cl),
# 										output = localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/simRes_kv", autoYes= TRUE),
# 										overwrite = TRUE)
# )
# timing