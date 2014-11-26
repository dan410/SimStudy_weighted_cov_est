### ON PIC ### 
setwd("~/Dissertation_projects/SimStudy_weighted_cov_est/")
### Local ### 
# setwd("/Users/fort002/Google Drive/Research/Projects/SimStudy_weighted_cov_est")


############################################################################
### packages
############################################################################
library(sseigfun) # my package for covariance estimation
library(sfdasim) # my package for simulating spatially distributed curves
library(geoR) # used to simulate gaussian random fields


############################################################################
### objects needed for simulation
############################################################################
sim_locs <- readRDS("Data/sim_locs.rds")

### True covariance function for the process
cov.true <- function(x,y){
	alpha = 2
	k <- 1:3
	res <- sum(k^(-2*alpha)*cos(k*pi*x)*cos(k*pi*y))
}


spatial_calc <- function(dep=0.001, grid_ID=1, weight=0.25, obs_per_curve=20){

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

############################################################################
### execute code in parallel
############################################################################

res	 <- clusterJob(X = 1:300,
                  FUN = spatial_calc,
                  applyFUN = 'parLapply',
                  account = 'PIC_ACCOUNT_HANDLE',
                  userName = 'fort002',
                  needed.objects = list('obj.fun', 'cov.true', 'sim_locs'),
                  numNodes = 2,
                  partition = 'short',
                  jobName = 'spatial_cov',
                  email.notification = 'daniel.fortin@pnnl.gov')
									
save(res, file = "sim_results.RData")



