


nsim = 300
sim_specs <- data.frame(dep = rep(3, nsim), grid_ID = rep(3, nsim), weight = rep(3, 100))
sim_specs$sim_ID <- 1:nrow(sim_specs)

args <- rbind( data.frame(dep = 0.05, ))