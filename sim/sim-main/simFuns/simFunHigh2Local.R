# n=150
# d=50
# tau=.3
# dtau=0
# distribution="normal"
# num_sim = 2
# clus = NULL
# r = 1

simFunHigh2Local <- function(n=100, d=10, tau=.5, delta=0, distribution="normal", num_sim = 250, filename = "resLow", clus = NULL, subset_dtau_type = NULL){
  
  # packages
  library(parallel)
  library(data.table)
  sapply(list.files("functionsHigh2/", full.names = T), source, local = environment())
  
  # create a parameter grid
  sim.grid <- createGrid(n, d, tau, delta, distribution, num_sim)
  
  if(!is.null(subset_dtau_type)){
    sim.grid <- sim.grid[sim.grid$dtau_type %in% subset_dtau_type,]
  }
  
  sim.grid$delta <- sim.grid$dtau
  sim.grid$dtau <- sim.grid$dtau/sqrt(sim.grid$n)
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
    library(HAC)
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
    
    cbind(sim.grid[r,],performTestsHigh2(X,M=M))
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(clus)
  return(NULL)
}
