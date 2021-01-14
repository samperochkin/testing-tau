# n=150
# d=50
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
  
  source("functions/computeTh.R")
  source("functions/buildSigma.R")
  source("functions/generateData.R")
  source("functions/testEquiRankJackknife.R")
  source("functions/testEquiRankWeird.R")
  clusterExport(clus, varlist=c("sim.grid",
                                "generateData",
                                "computeTh",
                                "buildSigma",
                                "testEquiRankJackknife",
                                "testEquiRankWeird"),envir = environment())
  
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
    test.jackknife <- testEquiRankJackknife(X,1000)

    res.grid <- sim.grid[rep(r,length(test.jackknife)),]
    res.grid[1:length(test.jackknife), `:=` (sigma = c(rep("jackknife",7),"bootstrap"), test = names(test.jackknife), pval = test.jackknife)]
    res.grid
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(clus)
  return(NULL)
}