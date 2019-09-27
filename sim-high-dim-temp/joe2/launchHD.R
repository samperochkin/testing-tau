# nohup R CMD BATCH launch.R log.txt &

source("functions/simFunMC.R")
# tiime <- Sys.time()

nd <- expand.grid(n = 150, d = 500)


for(r in 1:nrow(nd)){
  simFunMC(n = nd$n[r], d = nd$d[r], k = NULL, tau = c(0,.3,.6), dtau = c(0,.1,.2),
         distribution = "joe",
         num_sim = 10000,
         # num_sim = 1,
         filename = paste0("joe1/joe1_n",nd$n[r],"_d",nd$d[r]),
         clus = c(rep(c("dms1","dms2"),each=4), rep(c("dms3","dms4","dms5","dms6","dms7"),each=2), rep("dms11",each=8)))
  # clus = rep("dms11",4))
  # clus = NULL)
}
