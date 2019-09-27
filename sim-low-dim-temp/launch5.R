# nohup R CMD BATCH launch.R log.txt &


source("functions/simFun2.R")
# tiime <- Sys.time()
simFun(n = 100, d = 100, k = NULL, tau = .3, dtau = c(0,.1,.2,.3),
       distribution = "joe",
       num_sim = 10000,
       # num_sim = 1,
       filename = "joe1",
       clus = c(rep(c("dms1","dms2"),4), rep(c("dms3","dms4","dms5","dms6","dms7"),2), rep("dms11",12)))
       # clus = rep("dms11",4))
       # clus = NULL)
# tiime <- difftime(Sys.time(), tiime)

# 
# n = 100
# d = 20
# tau = .3
# dtau = 0
# distribution = "joe"
# num_sim = 1
# library(data.table)
# source("functions/createGrid.R")
# sim.grid <- createGrid(n, d, tau, dtau, distribution, 1)
# # n1 <- nrow(sim.grid)
# 
# sim.grid <- createGrid(n, d, tau, dtau, distribution, 10000)
# n2 <- nrow(sim.grid)
# 
# tiime*(n2/n1)/60/30
