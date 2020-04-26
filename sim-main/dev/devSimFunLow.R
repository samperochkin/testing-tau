n=100
d=5
tau=.3
dtau=0
dtau_type="none"
distribution="normal"
num_sim = 10
clus = NULL
M <- 5000

  # packages
  library(parallel)
  library(mvtnorm)
  library(data.table)
  sapply(list.files("sim-main/functionsLow/", full.names = T), source, local = environment())
  
  # create a parameter grid
  sim.grid <- createGrid(n, d, tau, dtau, distribution, num_sim)
  sim.grid$M <- 5000
  
  library(mvtnorm)

  res <- rbindlist(lapply(1:50, function(dummy){
    X <- generateData(n,d,tau,dtau,dtau_type,distribution)
    performTestsLow(X,M=M)
  }))

  res <- res[S == "I",]
  plot(sort(res[norm == "Supremum"]$pvalue))
  plot(sort(res[norm == "Euclidean"]$pvalue))

  
  
  
  mean(apply(SI.star %*% matrix(rnorm(M*p),p,M),2,crossprod) > loss  