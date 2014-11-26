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

grid <- subset(locs, grid == 1)

gp <- ggplot(grid, aes(x=x, y=y)) + 
geom_point() + 
theme_bw()+
labs(x = "", y = "")
gp

# ggsave("Plots/grid.pdf", height = 4, width = 5)

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
cov_df$range <- factor(cov_df$range)

gp <- ggplot(cov_df, aes(x = x, y = y, group=range, linetype = range, color = range)) + 
	geom_line()+
	theme_bw()+
  scale_color_manual(values = c("cornflowerblue", "darkolivegreen3", "firebrick1"))+
	labs(x = "", y = "correlation")+
	guides(color = guide_legend('range'), linetype = guide_legend('range'))
gp

# ggsave("Plots/exp_corr_funs.pdf", width = 8, height = 5)

############################################################################
### 
############################################################################

### Read in and rbind sim results
all_dat <- readRDS("Data/sim_res_grid.rds")
all_dat <- readRDS("Data/sim_res_india.rds")

### plot histograms of the distributions
gp <- ggplot(all_dat, aes(x = L2)) + 
geom_histogram()+
facet_wrap(dep~weight)+
xlim(c(0, 0.1))
gp

res_means <- ddply(subset(all_dat, L2 < 0.55), .(dep, weight), summarize, tmean = mean(L2), se = sd(L2)/50, n = 100 )
res_means <- subset(res_means, weight != 4)
res_ind <- subset(res_means, dep == 0.001)
res_means$dep <- factor(res_means$dep, levels = c(0.100, 0.200, 0.300, 0.001), labels=c( "0.1", "0.2", "0.3", "ind"))

# Define the top and bottom of the errorbars
limits <- aes(ymax = tmean + 2*se, ymin = tmean - 2*se)

gp <- ggplot(res_means, aes(x = weight, y = tmean, group = dep, color=dep, linetype = dep))+
geom_line() + geom_errorbar(limits, width=0.05, linetype = 1, color = "black") +
scale_color_manual(values = c( "cornflowerblue", "darkolivegreen3", "firebrick1", "black"))+
xlim(c(-0.05,1.05))+
theme_bw()+
labs(x="weight parameter, p", y = "mean integrated squared error", color = "spatial\ndependence\n(range)", linetype = "spatial\ndependence\n(range)")
gp

ggsave("Plots/MSE_trends.pdf", width = 8, height = 5)

res_med <- ddply(all_dat, .(dep, weight), summarize, med = median(L2), se = sd(L2)/length(L2), q1 = quantile(L2, probs = 0.4), q3 = quantile(L2, probs = 0.6), n = length(L2) )
res_med <- subset(res_med, weight != 4)
res_ind <- subset(res_med, dep == 0.001)

# Define the top and bottom of the errorbars
limits <- aes(ymax = med + q3, ymin = med - q1)

gp <- ggplot(subset(res_med, dep != 0.001), aes(x = weight, y = med, group = dep))+
geom_line(aes(color = dep), size = 1.5) + geom_errorbar(limits, width=0.1) +
geom_line(data = res_ind, aes(x = weight, y = med), linetype = "dashed", size = 2)+

#geom_point(aes( size = n), color = "orange")+
scale_size(range = c(2,4))+
labs(x="weight parameter, p", y = "mean integrated squared error", color = "spatial\ndependence\n(range)", size = "sample size")
gp


ggplot(subset(all_dat, dep != 0.4 & L2 < 0.1), aes(x = factor(weight), y = L2)) + 
	geom_boxplot() + 
	#geom_jitter() +
	facet_wrap(~dep)
	
# look at number of replicates for each of the treatment combinations	
ddply(all_dat, .(weight, dep), summarize, nrow = length(L2))
	