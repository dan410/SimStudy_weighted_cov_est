########################################################
#	create location configuration for simulation study
########################################################

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
#intensity <- c(rep(1, n1), rep(4, n2))
intensity <- est_intensity(locs, radius = 0.05)
df1 <- data.frame(x = locs[,1], y = locs[,2], intensity = intensity$intensity)
df1$grid <- 1



#############################################################################
#### create grid based on homogeneous agriculture locations in souther india
#############################################################################

### read in agriculture locs and scale to unit square
locs <- readRDS("Data/ag_locs.rds")[,c("locx", "locy")]/50

## estimate intensity
locs <- est_intensity(locs, radius = 0.05)

df2 <- data.frame(x = locs$locx, y  = locs$locy, intensity = locs$intensity)
df2$grid <- 2

sim_locs <- rbind(df1, df2)
# saveRDS(sim_locs, "Data/sim_locs.rds")

ggplot(sim_locs, aes(x = x, y = y, size = 1/intensity))+
geom_point()+
facet_wrap(~grid)


