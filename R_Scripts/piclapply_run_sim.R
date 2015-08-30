setwd('~/Dissertation_projects/SimStudy_weighted_cov_est')

##### MLE using piclapply #####
require(picRutils)

obj.fun <- function(dat, weight){
  
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
    return(NA)
  }, finally = {
  })
  ### estimate the L2 distance between estimated cov fun and true cov fun
  tt <- seq(0,1, length =  20)
  grid <- expand.grid(t1 = tt, t2 = tt)
  cov.true.pts <- mapply(cov.true, x = grid[,1], y=grid[,2])
  cov.est.pts <- mapply(sseigfun:::cov.fn, x = grid[,1], y=grid[,2], MoreArgs = list(knots=cov.est$knots, fit.obj=cov.est))
  dist.L2 <- sum((cov.true.pts - cov.est.pts)^2)/nrow(grid)
  
  res <- data.frame(L2 = dist.L2)
  return(res)
}

### true covariance function
cov.true <- function(x,y){
  alpha = 2
  k <- 1:3
  res <- sum(k^(-2*alpha)*cos(k*pi*x)*cos(k*pi*y))
}


### Select Grid ###
grid_ID <- 2

## read in object with different configuration of spatial locations
sim_locs <- readRDS("Data/sim_locs.rds")
intensity <- subset(sim_locs, grid == grid_ID)$intensity

DAT <- readRDS(paste("Data/GRID", grid_ID, ".rds", sep = ""))

# This will be a slow computation in serial, but we can do it quickly with
# piclapply. We will request 2 nodes to do our job (64 CPUs). Of course,
# when you do it, use your own account and your own PNNL e-mail address.

weight <- 0.8


L2 <- piclapply(DAT,
                obj.fun,
                weight = weight,
                account = 'spyglass',
                needed.objects = c('obj.fun', 'cov.true', 'intensity'),
                packages = "sseigfun",
                numNodes = 2,
                partition = 'short',
                time.limit.mins = 60,
                jobName = 'CovEstgrid2wt1',
                email.notification = 'daniel.fortin@pnnl.gov',
                verbose = TRUE)



save(L2, file = paste("piclapply_Res/grid", grid_ID, "_wt_", weight, ".RData", sep=""))