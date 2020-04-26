# n=250
# d=6
# design=c("unbalanced")
# dtau=0
# distribution="normal"
# num_sim = 2
# cl = NULL

simFunLowBlock <- function(n=100, d=6, design="balanced", dtau=0, distribution="normal", num_sim = 250, filename = "resLow", cl = NULL){
  
  # packages
  library(parallel)
  library(mvtnorm)
  library(data.table)
  sapply(list.files("functionsLowBlock/", full.names = T), source, local = environment())
  
  # create a parameter grid
  sim.grid <- createGrid(n, d, design, dtau, distribution, num_sim)
  sim.grid$M <- 5000

  # for parallel stuff
  if(is.null(cl)){
    cl <- makeCluster(detectCores()-1) 
  }else{
    cl <- makeCluster(cl)
  }
  
  clusterEvalQ(cl, expr={
    library(data.table)
    library(mvtnorm)
    # library(HAC)
  })
  
  clusterExport(cl,
                varlist=c("sim.grid",Filter(function(x) inherits(get(x), "function"), ls())),
                envir = environment())
  
  # simuls
  res <- rbindlist(parLapply(cl, sample(nrow(sim.grid)), function(r){
  # res <- rbindlist(lapply(1:nrow(sim.grid), function(r){
    # print(r)
    
    # assign parameters -- just for clarity
    n <- sim.grid[r,]$n
    d <- sim.grid[r,]$d
    design <- sim.grid[r,]$design
    dtau <- sim.grid[r,]$dtau
    dtau_type <- sim.grid[r,]$dtau_type
    distribution <- sim.grid[r,]$distribution
    M <- sim.grid[r,]$M
    
    X <- generateData(n,d,design,dtau,dtau_type,distribution)
    
    clus <- designToClus(d,design)
    cbind(sim.grid[r,],performTestsLowBlock(X,clus,M=M))
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(cl)
  return(NULL)
}