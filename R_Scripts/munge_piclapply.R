setwd("~/Dissertation_projects/SimStudy_weighted_cov_est/")

#############################################################################
### Grid 1
############################################################################
wts <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

grid1 <- NULL

for( wt in wts){ 
	load(paste("piclapply_Res/grid1_wt_", wt, ".RData", sep = ""))
	res <- data.frame(L2 = unlist(L2), range = rep(c(0,0.1, 0.2, 0.3, 0.4), each = 200))
	res$weight <- wt
	grid1 <- rbind(grid1, res)
	rm(L2)
}

saveRDS(grid1, "Data/grid1_res.rds")

#########################################################################
### Grid 2
#########################################################################

wts <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1)

grid2 <- NULL

for( wt in wts){ 
	load(paste("piclapply_Res/grid2_wt_", wt, ".RData", sep = ""))
	res <- data.frame(L2 = unlist(L2), range = rep(c(0,0.1, 0.2, 0.3, 0.4), each = 200))
	res$weight <- wt
	grid2 <- rbind(grid2, res)
	rm(L2)
}

saveRDS(grid2, "Data/grid2_res.rds")

#############################################################################
### Grid 3
############################################################################
wts <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

grid3 <- NULL

for( wt in wts){ 
	load(paste("piclapply_Res/grid3_wt_", wt, ".RData", sep = ""))
	res <- data.frame(L2 = unlist(L2), range = rep(c(0,0.1, 0.2, 0.3, 0.4), each = 200))
	res$weight <- wt
	grid3 <- rbind(grid3, res)
	rm(L2)
}

saveRDS(grid3, "Data/grid3_res.rds")
