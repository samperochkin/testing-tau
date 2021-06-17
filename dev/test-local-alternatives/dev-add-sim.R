# Double

res[, epsilon := NULL]
res[, decision := NULL]

res <- rbind(res,
             rbindlist(mclapply(sample(nrow(sim.grid)), function(r){
               
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
  
}, mc.cores = cores)))
