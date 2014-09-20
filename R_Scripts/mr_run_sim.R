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
	spatial_calc <- function(dep, grid_ID, weight, sim_locs){
	
		locs <- subset(sim_locs, grid == grid_ID)[, c("x","y")]
		intensity <- subset(sim_locs, grid == grid_ID)$intensity
	
		curves <- sim_sfda_curves(nBasis = 3, 
	                            type = "Cos", 
															basis.pars = 2,
	                            cov.model = rep("exponential", 3), 
	                            cov.pars = rbind(c(1, dep), c(1, dep), c(1, dep)),
	                            locs = locs)

		### generate observed data 
		m <- 5  # number of observations per curve
		times <- runif(m, min = 0.01, max = 0.99)
		dat <- with(curves, sim_sfda_data(locs = locs, coef = coef, basis.fns = basis.fns, sigma = 0.01, pts = times))
  
		# assign weights to each point based on the intensity values
		for( j in 1:length(unique(dat$ID))){
			dat$wt[dat$ID == j] <- (1/intensity[j])^(1/weight)
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
	
		res <- data.frame(dep = dep, grid_ID = grid_ID, weight = weight, L2 = dist.L2)
		return(res)

	}
	
})

############################################################################
### map
############################################################################

sim_map <- expression({
   for(i in 1:length(map.values)){
		 x <- map.values[[i]]
		 res <- spatial_calc(dep = x$dep, grid_ID = x$grid, weight = x$weight, sim_locs = sim_locs) 
		 collect(map.keys[[i]], res)
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

# create a 3 core cluster
library(parallel)
cl <- makeCluster(2)

sim_locs <- readRDS("Data/sim_locs.rds")

# execute the job
system.time(
sim_result_cl4 <- mrExec(bySimID, 
										params = list(sim_locs = sim_locs),
										setup = sim_setup, 
										map = sim_map, 
										reduce = sim_reduce,
										control = localDiskControl(cluster = cl))
)

#
#   user  system elapsed 
# 211.371   2.662 212.950 






