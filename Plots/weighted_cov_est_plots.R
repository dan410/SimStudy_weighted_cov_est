library(datadr) # map-reduce package
library(ggplot2)
library(plyr)

# set working directory
setwd("/Users/fort002/Google Drive/Research/Projects/SimStudy_weighted_cov_est")
### ON PIC ### setwd("~/Dissertation_projects/SimStudy_weighted_cov_est/")

############################################################################
### Plot spatial locations
############################################################################

locs <- readRDS("Data/sim_locs.rds")

grid3 <- subset(locs, grid == 3)

gp <- ggplot(grid3, aes(x=x, y=y)) + 
geom_point() + 
labs(x = "", y = "")
gp

ggsave("Plots/grid3.pdf", height = 4, width = 5)

library(geoR) # used to simulate gaussian random fields
library(spatstat) # simulating point processes

## first lets look at how covariance functions are specified in geoR
library(geoR)
cov.f <- function(x,...){
		cov.spatial(x, ...)
}
####### exponential covariance funcitons ##############
curve(cov.f(x, cov.model="exponential", cov.pars=c(1,.3)), from=0, to=1, main="exponential")
curve(cov.f(x, cov.model="exponential", cov.pars=c(1,.2)), from=0, to=1, add=TRUE, col="blue")
curve(cov.f(x, cov.model="exponential", cov.pars=c(1,.1)), from=0, to=1, add=TRUE, col="red")
###### Gaussian covariance functions #####################
curve(cov.f(x, cov.model="gaussian", cov.pars=c(1,.3)), from=0, to=1, main="gaussian")
curve(cov.f(x, cov.model="gaussian", cov.pars=c(1,.2)), from=0, to=1, add=TRUE, col="blue")
curve(cov.f(x, cov.model="gaussian", cov.pars=c(1,.1)), from=0, to=1, add=TRUE, col="red")

# plot exponential covariance functions
x <- seq(0,1, length = 1000)
exp_r3 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.3))
exp_r3_df <- data.frame(x = x, y = exp_r3, range = 0.3)

x <- seq(0,1, length = 1000)
exp_r2 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.2))
exp_r2_df <- data.frame(x = x, y = exp_r2, range = 0.2)

x <- seq(0,1, length = 1000)
exp_r1 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.1))
exp_r1_df <- data.frame(x = x, y = exp_r1, range = 0.1)

cov_df <- rbind(exp_r1_df, exp_r2_df, exp_r3_df)

gp <- ggplot(cov_df, aes(x = x, y = y, group = range)) + 
	geom_line(aes(color = factor(range)))+
	labs(x = "", y = "correlation", color = "range\nparameter")
gp

# ggsave("exp_corr_funs.pdf", height = 4, width = 5)

############################################################################
### 
############################################################################

### Read in and rbind sim results
data_files <- list.files("Data", pattern = "sim_res_Sep*")
data_files <- data_files[1:6]

all_dat <- NULL
for(i in seq_along(data_files)){
	all_dat <- rbind(all_dat, readRDS(file.path("Data", data_files[i])))
}

# test for duplicates
sum(duplicated(all_dat$L2))


res_means <- ddply(all_dat, .(dep, weight), summarize, tmean = mean(L2, trim = 0.2), se = sd(L2)/length(L2), n = length(L2) )
res_ind <- subset(res_means, dep == 0.001)

# Define the top and bottom of the errorbars
limits <- aes(ymax = tmean + se, ymin = tmean - se)

gp <- ggplot(subset(res_means, dep != 0.001), aes(x = weight, y = tmean, group = dep))+
geom_line(aes(color = dep), size = 1.5) + geom_errorbar(limits, width=0.1) +
geom_line(data = res_ind, aes(x = weight, y = tmean), linetype = "dashed", size = 2)+
geom_point(aes( size = n), color = "orange")+
scale_size(range = c(2,4))+
labs(x="weight parameter, p", y = "mean integrated squared error", color = "spatial\ndependence\n(range)", size = "sample size")
gp

ggsave("Plots/MSE_trends.pdf", width = 8, height = 5)


ggplot(subset(all_dat, dep != 0.4 & L2 < 0.1), aes(x = factor(weight), y = L2)) + 
	geom_boxplot() + 
	#geom_jitter() +
	facet_wrap(~dep)
	
# look at number of replicates for each of the treatment combinations	
ddply(all_dat, .(weight, dep), summarize, nrow = length(L2))
	