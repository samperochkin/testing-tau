simFun <- function(n=100, d=10, k=NULL, tau=.5, dtau=0, distribution="normal", num_sim = 250, filename = "res", clus = NULL){
  
  # packages
  require(parallel)
  require(data.table)
  require(mvtnorm)
  require(copula)
  require(HAC)
  require(pcaPP)
  require(Matrix)
  
  # If parameter k is set, compute n or d as the proper ratio
  if(!is.null(k)){
    if(is.null(d)){
      d <- n/k
    }else{
      n <- k/d
    }
  }
  
  # create a parameter grid
  source("functions/createGrid.R")
  sim.grid <- createGrid(n, d, tau, dtau, distribution, num_sim)

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
    library(pcaPP)
    library(Matrix)
  })
  
  source("functions/generateData.R")
  source("functions/testEquiRankMC-expansion.R")
  clusterExport(clus, varlist=c("sim.grid",
                                "generateData", "testEquiRankMC"),envir = environment())
  
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
    
    X <- generateData(n,d,tau,dtau,dtau_type,distribution)
    test <- testEquiRankMC(X,500)

    res.grid <- sim.grid[rep(r,3),]
    res.grid[, `:=` (method = substr(names(test),6,15), pval = test)]
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(clus)
  return(NULL)
}
