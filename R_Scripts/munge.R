library(sseigfun) # my package for covariance estimation#
library(sfdasim) # my package for simulating spatially distributed curves
library(geoR) # used to simulate gaussian random fields
library(spatstat) # simulating point processes
library(datadr) # map-reduce package
library(ggplot2)
library(plyr)
### ON PIC ### setwd("~/Dissertation_projects/SimStudy_weighted_cov_est/")
### Local ### setwd("/Users/fort002/Google Drive/Research/Projects/SimStudy_weighted_cov_est")


res <- ddf(localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/simRes_kv"), update = TRUE)

res <- ddf(localDiskConn("~/Dissertation_projects/Map_files/Weighted_cov_kv/sim_res_grid3_dep3"), update = TRUE)

res_df <- res[[1]][[2]]
head(res_df)


file_stamp <- format(Sys.time(), "%b-%d-%H-%M-%S")
saveRDS(res_df, paste("Data/sim_res_", file_stamp, ".rds", sep=""))
rm(file_stamp)


ggplot(res_df, aes(x = factor(weight), y = L2)) + geom_boxplot() + ylim(c(0,0.1))

### Read in and rbind sim results
data_files <- list.files("Data", pattern = "sim_res_Sep*")
#data_files <- data_files[7:9]

all_dat <- NULL
for(i in seq_along(data_files)){
	all_dat <- rbind(all_dat, readRDS(file.path("Data", data_files[i])))
}

dat1 <- readRDS(file.path("Data", data_files[1]))
dat2 <- readRDS(file.path("Data", data_files[2]))
dat3 <- readRDS(file.path("Data", data_files[3]))
dat4 <- readRDS(file.path("Data", data_files[4]))
dat5 <- readRDS(file.path("Data", data_files[5]))
dat6 <- readRDS(file.path("Data", data_files[6]))
dat7 <- readRDS(file.path("Data", data_files[7]))
dat8 <- readRDS(file.path("Data", data_files[8]))
dat9 <- readRDS(file.path("Data", data_files[9]))

# test for duplicates
sum(duplicated(all_dat$L2))


res_means <- ddply(dat, .(dep, weight), summarize, tmean = mean(L2, trim = 0.2) )

ggplot(subset(all_dat, dep != 0.4 & L2 < 0.1), aes(x = factor(weight), y = L2)) + 
	geom_boxplot() + 
	#geom_jitter() +
	facet_wrap(~dep)
	
# look at number of replicates for each of the treatment combinations	
ddply(all_dat, .(weight, dep), summarize, nrow = length(L2))
	