library(sseigfun) # my package for covariance estimation
library(sfdasim) # my package for simulating spatially distributed curves
library(geoR) # used to simulate gaussian random fields
library(spatstat) # simulating point processes
library(datadr) # map-reduce package

### ON PIC ### setwd("~/Dissertation_projects/SimStudy_weighted_cov_est/")
### Local ### setwd("/Users/fort002/Google Drive/Research/Projects/SimStudy_weighted_cov_est")


res <- ddf(localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/simRes_kv"), update = TRUE)

res <- ddf(localDiskConn("~/Documents/Projects/Dissertation_kv/Weighted_cov_kv/simRes_kv"), update = TRUE)

res_df <- res[[1]][[2]]

ggplot(res_df, aes(x = factor(weight), y = L2)) + geom_boxplot() + ylim(c(0,0.1))

saveRDS(res_df, "Data/sim_res_.rds")

