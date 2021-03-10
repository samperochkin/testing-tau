# n=100
# d=10
# tau=.5
# epsilon=1
# distribution="normal"
# num_sim = 2

simFunLocal <- function(n=100, d=10, tau=.5, epsilon=1, distribution="normal", num.sim = 250, filename = "resLocal", cores = 1){
  
  # packages
  library(parallel)
  library(mvtnorm)
  library(data.table)
  sapply(list.files("functionsLocal/", full.names = T), source, local = environment())
  
  # create a parameter grid
  sim.grid <- createGrid(n, d, tau, epsilon, distribution, num.sim)
  sim.grid <- sim.grid[dtau_type == "single"]
  sim.grid$M <- 2500
  
  # for parallel stuff
  res <- rbindlist(mclapply(sample(nrow(sim.grid)), function(r){
    
    # assign parameters -- just for clarity
    n <- sim.grid[r,]$n
    d <- sim.grid[r,]$d
    p <- choose(d,2)
    tau <- sim.grid[r,]$tau
    epsilon <- sim.grid[r,]$dtau
    dtau_type <- sim.grid[r,]$dtau_type
    distribution <- sim.grid[r,]$distribution
    M <- sim.grid[r,]$M
    
    X <- generateData(n,d,tau,epsilon/sqrt(n),dtau_type,distribution)
    
    # bias term
    epsilon.vec <- epsilon*tau/p * c(p-1,rep(-1,p-1))
    
    cbind(sim.grid[r,],performTestsAlternative(X,epsilon.vec,M=M))
  }, mc.cores = cores))
  
  fwrite(res, paste0(filename,".csv"))
  return(NULL)
}
