computeHajekProjection <- function(X, ij.mat=NULL){
  n <- nrow(X)
  
  sapply(1:n, function(r){
    V <- t(X[r,] < t(X[-r,]))
    V <- Reduce("+",lapply(1:nrow(V), function(k){
      4*(outer(V[k,],V[k,],"=="))
    }))/sqrt(n*(n-1))
    if(!is.null(ij.mat)) return(V[ij.mat])
    V
  }, simplify = !is.null(ij.mat))
}