buildSigma <- function(Theta, tau, n, diagOnly=TRUE){
  if(diagOnly) tau.mod <- 2*(2*n - 3) / (n*(n - 1)) * (tau + 1)^2
  else tau.mod <- 2*(2*n - 3) / (n*(n - 1)) * outer(tau+1,tau+1,"*")
  
  Theta - tau.mod
}
