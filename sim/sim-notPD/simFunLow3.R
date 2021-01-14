# n=150
# d=5
# tau=.3
# dtau=0
# distribution="normal"
# num_sim = 2
# clus = NULL

simFunLow3 <- function(n=100, d=10, tau=.5, dtau=0, distribution="normal", num_sim = 250, filename = "resLow", clus = NULL){
  
  # packages
  library(parallel)
  library(mvtnorm)
  library(data.table)
  sapply(list.files("functionsLow3/", full.names = T), source, local = environment())
  
  # create a parameter grid
  sim.grid <- createGrid(n, d, tau, dtau, distribution, num_sim)
  sim.grid$M <- 5000

  # for parallel stuff
  if(is.null(clus)){
    clus <- makeCluster(detectCores()-1) 
  }else{
    clus <- makeCluster(clus)
  }
  
  clusterEvalQ(clus, expr={
    library(data.table)
    library(mvtnorm)
    # library(HAC)
  })
  
  clusterExport(clus,
                varlist=c("sim.grid",Filter(function(x) inherits(get(x), "function"), ls())),
                envir = environment())
  
  # simuls
  res <- rbindlist(parLapply(clus, sample(nrow(sim.grid)), function(r){
  # res <- rbindlist(lapply(1:nrow(sim.grid), function(r){
    # print(r)
    
    # assign parameters -- just for clarity
    n <- sim.grid[r,]$n
    d <- sim.grid[r,]$d
    tau <- sim.grid[r,]$tau
    dtau <- sim.grid[r,]$dtau
    dtau_type <- sim.grid[r,]$dtau_type
    distribution <- sim.grid[r,]$distribution
    M <- sim.grid[r,]$M
    
    X <- generateData(n,d,tau,dtau,dtau_type,distribution)
    
    cbind(sim.grid[r,],performTestsLow3(X,M=M))
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(clus)
  return(NULL)
}