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
  
  source("functions/generateData.R")
  source("functions/testEquiRankJackknife.R")
  source("functions/testEquiRankWeird.R")
  clusterExport(clus, varlist=c("sim.grid",
                                "generateData",
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
    test.jackknife <- testEquiRankJackknife(X,4000)
    test.weird <- testEquiRankWeird(X,4000)
    
    modified.jackknife <- test.jackknife["modified"]
    test.jackknife <- test.jackknife[-which(names(test.jackknife)=="modified")]
    
    res.grid <- sim.grid[rep(r,length(c(test.jackknife,test.weird))),]
    res.grid[1:length(test.jackknife), `:=` (sigma = c(rep("jackknife",8),"bootstrap"), test = names(test.jackknife), pval = test.jackknife, modified = modified.jackknife)]
    res.grid[(1+length(test.jackknife)):length(c(test.jackknife,test.weird)), `:=` (sigma = "weird", test = names(test.weird), pval = test.weird, modified = NA)]
    res.grid
  }))
  
  fwrite(res, paste0(filename,".csv"))
  stopCluster(clus)
  return(NULL)
}