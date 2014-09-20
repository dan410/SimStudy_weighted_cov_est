

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
	m <- 20  # number of observations per curve
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




