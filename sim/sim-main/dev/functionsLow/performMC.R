performMC <- function(loss, S.star, norm, M){
  p <- ncol(S.star)
  # if(norm == "Euclidean") return(mean(apply(S.star %*% matrix(rnorm(M*p),p,M),2,crossprod) > loss))
  # if(norm == "Supremum") return(mean(apply(abs(S.star %*% matrix(rnorm(M*p),p,M)),2,max) > loss))
  if(norm == "Euclidean") return(mean(apply(rmvnorm(M,rep(0,p),S.star),1,crossprod) > loss))
  if(norm == "Supremum") return(mean(apply(abs(rmvnorm(M,rep(0,p),S.star)),1,max) > loss))
}
