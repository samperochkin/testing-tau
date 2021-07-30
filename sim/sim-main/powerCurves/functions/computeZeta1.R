computeZeta1 <- function(zeta1.line, N){
  d <- zeta1.line$d
  p <- choose(d,2)
  distribution <- zeta1.line$distribution
  dtau_type <- zeta1.line$dtau_type
  
  if(distribution %in% c("normal", "t4")){
    if(dtau_type == "single") h <- c(1,rep(0,p-1)) # ---------------------------- ASK JOHANNA, CHANGE FOR HN IF NEEDED
    if(dtau_type == "column") h <- c(rep(0,d-1),rep(1,p-d+1)) # ----------------- SAME
    
    h <- h*cos(pi*zeta1.line$tau/2)*pi/2
    
  }else if(distribution %in% c("clayton", "gumbel")){
    h <- c(0,1)  # --------------------------------------------------------------- SAME
    if(distribution == "clayton") h <- h*2/(1-zeta1.line$tau)^2
    if(distribution == "gumbel") h <- h/(1-zeta1.line$tau)^2
  }
  
  # generate X
  tau <- zeta1.line$tau
  X <- generateData(n=N, d=d, tau=tau, dtau=0, dtau_type="none", distribution=distribution)
  
  gs <- gFun(X, distribution, tau)
  scores <- scoreFunction(X, distribution, tau, dtau_type)
  c((Reduce("+", lapply(1:N, function(s) 2 * tcrossprod(gs[s,],scores[s,])))/N) %*% h)
}
