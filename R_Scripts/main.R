library(sseigfun) # my package for covariance estimation
library(sfdasim) # my package for simulating spatially distributed curves
library(geoR) # used to simulate gaussian random fields
library(spatstat) # simulating point processes
library(datadr) # map-reduce package

# source R functions
path = "R"
for (nm in list.files(path, pattern = "[.][Rr]$")) {
       source(file.path(path, nm))
}

# read in set of location configurations to use 
sim_locs <- readRDS("Data/sim_locs.rds")

### using datadr 
nsim = 6*10
sim_specs <- data.frame(dep = rep(0.2, nsim), grid_ID = rep(3, nsim), weight = rep(1:6, each=nsim/6))
sim_specs$sim_ID <- 1:nrow(sim_specs)

# create a k/v pair for each row 
bySimID <- divide(sim_specs, by = c("sim_ID"), update = TRUE)

# define a function which will be called with the arguments corresponding to each row
covf_stat <- function(x){
	res <- spatial_calc(dep = x$dep, grid_ID = x$grid, weight = x$weight, sim_locs = sim_locs)
}

bySimID_stat <- addTransform(bySimID, covf_stat)

# compute and combine results
sim_result <- recombine(bySimID_stat, combRbind)



ggplot(sim_result, aes(x = factor(weight), y = L2)) + 
	geom_boxplot() + 
	facet_wrap(~dep) 
	#ylim(c(0,.15))
	

means <- ddply(sim_result, .(weight), function(x) mean(x$L2, trim = 0.00))

# saveRDS(sim_result, "Data/sim_result3.rds")

df <- readRDS("Data/sim_result4")
res <- rbind(readRDS("Data/sim_result2.rds"), readRDS("Data/sim_result.rds"), readRDS("Data/sim_result3.rds"))