library(parallel)

cl <- makeCluster(detectCores()-1)

clusterEvalQ(cl,{
  library(mvtnorm)
  library(data.table)
  
  Tau <- readRDS("sim-main/fromApplication/Tt.rds")
  clus <- readRDS("sim-main/fromApplication/clus.rds")
  ij.mat <- readRDS("sim-main/fromApplication/ijmat.rds")
  B <- readRDS("sim-main/fromApplication/B.rds")
  n <- 65
  d <- nrow(Tau)

  M <- 5000
  source("sim-main/fromApplication/blockTest.R")
  source("sim-main/functionsLow2/performMC2.R")
  source("sim-main/functionsLow2/computeHajekProjection.R")
  source("sim-main/functionsLow2/computeSigmaPlugin.R")
  source("sim-main/functionsLow2/computeSigmaJackknife.R")
})

t0 <- Sys.time()
dt <- rbindlist(parLapply(cl,1:2500,function(id){
  res <- blockTest(X = rmvnorm(n,rep(0,d),sin(Tau*pi/2)),B,ij.mat,M)
  res$id <- id
  res
}))
difftime(Sys.time(),t0)

stopCluster(cl)
fwrite(dt,"sim-main/results/dt_main_app.csv")