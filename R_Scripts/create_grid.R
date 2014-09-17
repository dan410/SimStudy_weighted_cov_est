########################################################
#	create location configuration for simulation study
########################################################

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

### grid1
pts1 <- grid_points(6)
pts1 <- subset(pts1, Var1 > 0.61 | Var2 > 0.61)
pts2 <- grid_points(6, a=0, b=0.6)
n1 <- nrow(pts1)
n2 <- nrow(pts2)
locs1 <- rbind(pts1, pts2)
intensity <- c(rep(1, n1), rep(2.78, n2))
df1 <- data.frame(x = locs1[,1], y = locs1[,2], intensity = intensity)
df1$grid <- 1

### grid 2
pts1 <- grid_points(6)
pts1 <- subset(pts1, Var1 > 0.4 | Var2 > 0.4)
pts2 <- grid_points(6, a=0, b=0.4)
n1 <- nrow(pts1)
n2 <- nrow(pts2)
locs <- rbind(pts1, pts2)
intensity <- c(rep(1, n1), rep(4, n2))
df2 <- data.frame(x = locs[,1], y = locs[,2], intensity = intensity)
df2$grid <- 2

### grid 3
pts1 <- grid_points(6)
pts1 <- subset(pts1, Var1 > 0.2 | Var2 > 0.2)
pts2 <- grid_points(6, a=0, b=0.2)
n1 <- nrow(pts1)
n2 <- nrow(pts2)
locs <- rbind(pts1, pts2)
intensity <- c(rep(1, n1), rep(4, n2))
df3 <- data.frame(x = locs[,1], y = locs[,2], intensity = intensity)
df3$grid <- 3

sim_locs <- rbind(df1, df2, df3)

# saveRDS(sim_locs, "Data/sim_locs.rds")

ggplot(sim_locs, aes(x = x, y = y, color = intensity))+
geom_point()+
facet_wrap(~grid)


