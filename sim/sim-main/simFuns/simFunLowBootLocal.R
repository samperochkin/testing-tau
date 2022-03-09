# n=100
# d=10
# tau=.5
# dtau=0
# distribution="normal"
# num_sim = 2
# clus = NULL

simFunLowBootLocal <- function(n=100, d=10, tau=.5, delta=0, distribution="normal", num_sim = 250, filename = "resLow", clus = NULL){
  
  # packages
  library(parallel)
  library(mvtnorm)
  library(data.table)
  sapply(list.files("functionsLowBoot/", full.names = T), source, local = environment())
  
  # create a parameter grid
  sim.grid <- createGrid(n, d, tau, delta, distribution, num_sim)
  sim.grid$delta <- sim.grid$dtau
  sim.grid$dtau <- sim.grid$dtau/sqrt(sim.grid$n)
  sim.grid$M <- 5000
  
  # print("hey")
  # for parallel stuff
  if(is.null(clus)){
    clus <- makeCluster(detectCores()-1) 
  }else{
    clus <- makeCluster(clus)
  }
  
  # print("got here")
  clusterEvalQ(clus, expr={
    library(data.table)
    library(mvtnorm)
    library(HAC)
  })
  
  # print("got there")
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
    
    cbind(sim.grid[r,],performTestsLowBoot(X,M=M))
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(clus)
  return(NULL)
}