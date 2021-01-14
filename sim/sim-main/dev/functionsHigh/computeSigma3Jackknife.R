computeSigma3Jackknife <- function(Ths,Tcs,Tbs){
  
  p <- nrow(Ths)
  d <- nrow(Tcs)
  sigma.hat <- c(
    var(Tbs),
    mean(apply(Tcs, 1, var)),
    mean(apply(Ths, 1, var))
  )
  
  c(sigma.hat %*% matrix(c(
    c(p,-2*(d-1),1)/(p-2*d+3),
    0,(d-1)/(d-2),-1/(d-2),
    0,0,1),3,3))
}
