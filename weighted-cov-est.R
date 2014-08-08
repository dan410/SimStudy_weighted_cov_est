### Investigating the effect of adding weights to the covariance estimation proposed by Cai&Yuan. The
### weights are determined by the spatial distribution of curve locations.

library(sseigfun) #my package for covariance estimation
library(sfdasim) # my package for simulating spatially distributed curves
library(geoR) # used to simulate gaussian random fields
library(spatstat) # simulating point processes

##########################################################################
## first lets look at how covariance functions are specified in geoR
##########################################################################
cov.f <- function(x,...){
		cov.spatial(x, ...)
}
####### exponential covariance funcitons ##############
curve(cov.f(x, cov.model="exponential", cov.pars=c(1,.3)), from=0, to=1, main="exponential")
curve(cov.f(x, cov.model="exponential", cov.pars=c(1,.2)), from=0, to=1, add=TRUE, col="blue")
curve(cov.f(x, cov.model="exponential", cov.pars=c(1,.1)), from=0, to=1, add=TRUE, col="red")
curve(cov.f(x, cov.model="exponential", cov.pars=c(1,.05)), from=0, to=1, add=TRUE, col="gray")
###### Gaussian covariance functions #####################
curve(cov.f(x, cov.model="gaussian", cov.pars=c(1,.3)), from=0, to=1, main="gaussian")
curve(cov.f(x, cov.model="gaussian", cov.pars=c(1,.2)), from=0, to=1, add=TRUE, col="blue")
curve(cov.f(x, cov.model="gaussian", cov.pars=c(1,.1)), from=0, to=1, add=TRUE, col="red")

##############################################
#     plot the true covariance function      #
##############################################
cov.true <- function(x,y){
	alpha = 2
	k <- 1:3
	res <- sum(k^(-2*alpha)*cos(k*pi*x)*cos(k*pi*y))
}
tt <- seq(0,1, length =  40)
grid <- expand.grid(t1 = tt, t2 = tt)
surface <- mapply(cov.true, x = grid[,1], y=grid[,2])
wireframe(surface ~ grid[,1]*grid[,2], drape=TRUE, pretty=TRUE, 	scales=list(arrows=FALSE), xlab='', ylab='',zlab='', zlim=c(-1.2,1.2))

# look at the diagonal C(t,t)
diag <- mapply(cov.true, x = grid[,1], y=grid[,1])
plot(grid[,1], diag, type = "l", ylim = c(0, 1.2))

#####################################################################
#    simulate curves with spatial structure on the coefficients     #
#####################################################################
mycurves <- sim_sfda_curves(nBasis=3, 
                            type="Cos", 
                            cov.model=rep("exponential",3), 
                            cov.pars=rbind(c(1,.1), c(1,0.1), c(1,0.1)),
														basis.pars = 2,
                            grid.dim=c(7,7),
                            grid.xlim=c(0,1),
                            grid.ylim=c(0,1))
                            
with(mycurves, plot_curves( coef = coef, basis.fns, ylim = c(-2,2)))

### generate observed data 
m <- 10  # number of observations per curve
times <- seq(0.05,0.95, length = m)
dat <- with(mycurves, sim_sfda_data(locs = locs, coef = coef, basis.fns = basis.fns, sigma = 0.01, pts = times))

############################################################
# Simulate locations with a specified intensity function.
# We could use the intensity function or a density estimate
# for the weights in the covariance estimation.
############################################################

W <- square(r=1) # unit square window object
# define the intesity function
lamfun <- function(x,y){
	stopifnot(inside.owin(x,y,W))
	res <- rep(2*40, length=length(x))
	res[x > 0.5] <- 2*10 
	return(res)
}
sim.pp <- rpoispp(lamfun, lmax=1000, win=W)
n.locs <- sim.pp$n
# automatic bandwidth selection
bw <- bw.diggle(sim.pp)
plot(density(sim.pp, sigma=bw))
# user chosen bandwidth 
bw <- 0.3
plot(density(sim.pp, sigma=bw))

# evaluate intensity at points
plot(density(sim.pp, sigma = bw, at="points"))
den <- density(sim.pp, sigma = bw, at="points")
lamwt <- mapply(lamfun, x = sim.pp$x, y = sim.pp$y)
loc.wts <- data.frame(x = sim.pp$x, y = sim.pp$y, int = den[1:n.locs], sqt.int = sqrt(den[1:n.locs]), lambda = lamwt)


################################################################################################
#    simulate curves with spatial structure on the coefficients using the point pattern data   
###############################################################################################
mycurves <- sim_sfda_curves(nBasis=3, 
                            type="Cos", 
														basis.pars = 2,
                            cov.model=rep("exponential",3), 
                            cov.pars=rbind(c(1,.3), c(1,0.3), c(1,0.3)),
                            locs=loc.wts[,c("x","y")])
                            
with(mycurves, plot_curves( coef = coef, basis.fns, ylim = c(-2,2)))

### generate observed data 
m <- 5  # number of observations per curve
times <- seq(0.05,0.95, length = m)
dat <- with(mycurves, sim_sfda_data(locs = locs, coef = coef, basis.fns = basis.fns, sigma = 0.01, pts = times))

# set the weights
dat$wt <- 1

for( i in 1:length(unique(dat$ID))){
	dat$wt[dat$ID == i] <- 1/(loc.wts[i,c("int")])^(1.5/1)
}




########################################################
# estimate the covariance function using weights
########################################################
cov.est <- estimate_cov_function(dat, n.marginal.knots=5)
#plot_covfit(cov.est)

### estimate the L2 distance between estimated cov fun and true cov fun
tt <- seq(0,1, length =  20)
grid <- expand.grid(t1 = tt, t2 = tt)
cov.true.pts <- mapply(cov.true, x = grid[,1], y=grid[,2])
cov.est.pts <- mapply(sseigfun:::cov.fn, x = grid[,1], y=grid[,2], MoreArgs = list(knots=cov.est$knots, fit.obj=cov.est))
dist.L2 <- sum((cov.true.pts - cov.est.pts)^2)


eigen.fns <- estimate_eigenfunctions(cov.fit=cov.est)
 

evals <- eigen.fns$values

par(mfrow=c(1,2))
xs <- seq(0,1, length=100)

ef1 <- eigen.fns$fns[[1]]
plot(xs, 1*ef1(xs)*sqrt(evals[1]), type="l", ylim=c(-2,2))
points(xs, -1*mycurves$basis.fns[[1]](xs), type="l", lty=2)

ef2 <- eigen.fns$fns[[2]]
plot(xs, ef2(xs)*sqrt(sort(evals, decreasing=TRUE)[2]), type="l", ylim=c(-1,1))
points(xs, mycurves$basis.fns[[2]](xs), type="l", lty=2)



















