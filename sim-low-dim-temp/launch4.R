# nohup R CMD BATCH launch.R log.txt &


source("functions/simFun2.R")
# tiime <- Sys.time()

nd <- expand.grid(n = seq(100,400,100), d = seq(5,20,5))
nd <- nd[-1,]


for(r in 1:nrow(nd)){
  simFun(n = nd$n[r], d = nd$d[r], k = NULL, tau = c(0,.3,.6), dtau = c(0,.1,.2),
         distribution = "joe",
         num_sim = 10000,
         # num_sim = 1,
         filename = paste0("joe1/joe1_n",nd$n[r],"_d",nd$d[r]),
         clus = c(rep(c("dms1","dms2"),4), rep(c("dms3","dms4","dms5","dms6","dms7"),2), rep("dms11",8)))
  # clus = rep("dms11",4))
  # clus = NULL)
}

# tiime <- difftime(Sys.time(), tiime)

# 
# n = seq(100,400,100)
# d = seq(5,20,5)
# n = 400
# d = 20
# tau = c(0,.3,.6)
# dtau = c(0,.1,.2)
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
