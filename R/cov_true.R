

### True covariance function for the process
cov.true <- function(x,y){
	alpha = 2
	k <- 1:3
	res <- sum(k^(-2*alpha)*cos(k*pi*x)*cos(k*pi*y))
}