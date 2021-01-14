library(parallel)
library(data.table)
library(mvtnorm)
library(pcaPP)


source("functions/generateData.R")
source("functions/testEquiRankJackknife.R")


clus <- makeCluster(c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4))) 
clusterEvalQ(clus, expr={
  library(data.table)
  library(mvtnorm)
  library(pcaPP)
})

clusterExport(clus, varlist=c("generateData",
                              "testEquiRankJackknife"),envir = environment())


dt <- rbindlist(parSapply(clus, 1:2000, function(dummy){
  n <- 100
  d <- 50
  tau <- .5
  dtau <- 0
  dtau_type <- "none"
  distribution <- "normal"
  
  X <- generateData(n,d,tau,dtau,dtau_type,distribution)
  data.table(cor = c(F,T), testEquiRankJackknife(X,2000))
}, simplify = F))

stopCluster(clus)

fwrite(dt, "dt_extra_1.csv")

