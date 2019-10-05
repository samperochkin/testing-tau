# n=100
# d=10
# k=NULL
# tau=.5
# dtau=0
# distribution="normal"
# num_sim = 2


simFun <- function(n=100, d=10, k=NULL, tau=.5, dtau=0, distribution="normal", num_sim = 250, filename = "res", clus = NULL){
  
  # packages
  require(parallel)
  require(data.table)

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
  source("functions/testEquiRankJackknife.R")
  source("functions/testEquiRankJackknifeCor.R")
  clusterExport(clus, varlist=c("sim.grid",
                                "generateData",
                                "testEquiRankJackknife",
                                "testEquiRankJackknifeCor"),envir = environment())
  
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
    test.jackknife <- testEquiRankJackknife(X,4000)
    test.jackknife.cor <- testEquiRankJackknifeCor(X,4000)
    
    res.grid <- sim.grid[rep(r,4*ncol(test.jackknife)),]
    res.grid[1:ncol(test.jackknife), `:=` (sigma = "jackknife", correction = FALSE, force_constraints = FALSE, test = colnames(test.jackknife), pval = test.jackknife[1,])]
    res.grid[ncol(test.jackknife) + 1:ncol(test.jackknife), `:=` (sigma = "jackknife", correction = FALSE, force_constraints = TRUE, test = colnames(test.jackknife), pval = test.jackknife[2,])]
    res.grid[2*ncol(test.jackknife) + 1:ncol(test.jackknife), `:=` (sigma = "jackknife", correction = TRUE, force_constraints = FALSE, test = colnames(test.jackknife), pval = test.jackknife.cor[1,])]
    res.grid[3*ncol(test.jackknife) + 1:ncol(test.jackknife), `:=` (sigma = "jackknife", correction = TRUE, force_constraints = TRUE, test = colnames(test.jackknife), pval = test.jackknife.cor[2,])]
    res.grid
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(clus)
  return(NULL)
}