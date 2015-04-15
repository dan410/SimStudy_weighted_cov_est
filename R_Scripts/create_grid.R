########################################################
#	create location configuration for simulation study
########################################################

### ON PIC ### 
setwd("~/Dissertation_projects/SimStudy_weighted_cov_est/")
### Local ### 
# setwd("/Users/fort002/Google Drive/Research/Projects/SimStudy_weighted_cov_est")

source("R/est_intensity.R")

### here is a funciton I created to make grid point creation easier
grid_points <- function(n, a = 0, b = 1, adj=FALSE){
	if(adj){		
		spc <- (b-a)/(n+1)
		xs <- seq(from=a + spc, to=b-spc, by = spc)
	}else{
		xs <- seq(from=a, to=b, length=n)
	}
	grid.pts <- expand.grid(xs , xs)
	return(grid.pts)
}


### grid 1
pts1 <- grid_points(6)
pts1 <- subset(pts1, Var1 > 0.2 | Var2 > 0.2)
pts2 <- grid_points(6, a=0, b=0.2)
n1 <- nrow(pts1)
n2 <- nrow(pts2)
locs <- rbind(pts1, pts2)
names(locs) <- c("locx", "locy")
intensity <- est_intensity(locs, radius = 0.05)
df1 <- data.frame(x = locs[,1], y = locs[,2], intensity = intensity$intensity)
df1$grid <- 1


### grid 2
pts1 <- grid_points(6)
pts1 <- subset(pts1, Var1 > 0.2 | Var2 > 0.2)
pts2 <- grid_points(6, a=0, b=0.1)
n1 <- nrow(pts1)
n2 <- nrow(pts2)
locs <- rbind(pts1, pts2)
names(locs) <- c("locx", "locy")
#intensity <- c(rep(1, n1), rep(4, n2))
intensity <- est_intensity(locs, radius = 0.05)
df2 <- data.frame(x = locs[,1], y = locs[,2], intensity = intensity$intensity)
df2$grid <- 2
ggplot(df2, aes(x=x, y=y, size = intensity)) + geom_point()

### grid 3
pts1 <- grid_points(6)
pts1 <- subset(pts1, Var1 >= 0.5 )
pts2 <- grid_points(12)
pts2 <- subset(pts2, Var1 < 0.4 )
n1 <- nrow(pts1)
n2 <- nrow(pts2)
locs <- rbind(pts1, pts2)
names(locs) <- c("locx", "locy")
#intensity <- c(rep(1, n1), rep(4, n2))
intensity <- est_intensity(locs, radius = 0.15)
df3 <- data.frame(x = locs[,1], y = locs[,2], intensity = intensity$intensity)
df3$grid <- 3
ggplot(df3, aes(x=x, y=y, size = intensity)) + geom_point()



grids <- rbind(df1, df2, df3)
saveRDS(grids, "Data/sim_locs.rds")

