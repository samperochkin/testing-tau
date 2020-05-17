constructTauHajek <- function(X){
  n <- nrow(X)

  lapply(1:n, function(r){
    v <- t(X[r,] < t(X[-r,]))
    Reduce("+",lapply(1:nrow(v), function(k){
      4*(outer(v[k,],v[k,],"=="))
    }))
  })
}
