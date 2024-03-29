performMC2 <- function(loss, S.star, norm, M, take.square.root=F){
  p <- ncol(S.star)
  
  if(take.square.root == T){
    # watch out with that
    ev <- eigen(S.star, symmetric = TRUE)
    S.star <- ev$vectors %*% (t(ev$vectors) * sqrt(pmax(ev$values, 0)))
  }
  
  if(norm == "Euclidean") return(mean(apply(matrix(rnorm(M*p,0,1),M,p) %*% t(S.star),1,crossprod) > loss))
  if(norm == "Supremum") return(mean(apply(abs(matrix(rnorm(M*p,0,1),M,p) %*% t(S.star)),1,max) > loss))
}
