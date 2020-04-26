library(matrixcalc)

createMatrix <- function(d, type, coeff, min.coeff = 0){
  if(d == 1) return(matrix(1,1,1))
  
  if(type == "equi" | d == 2){
    return((1-coeff)*diag(d) + coeff)
  }
  
  if(type == "un1"){
    M <- matrix(0,d,d)
    for(i in 1:(d-1)){
      M[i,(i+1):d] <- pmax(coeff^(1:(d-i)),min.coeff)
    }
    M <- diag(d) + M + t(M)
    if(!is.positive.definite(sin(pi*M/2))) print("wowowo mister")
    return(M)
  }
  
  if(type == "un2"){
    M <- matrix(0,d,d)
    for(i in 1:(d-1)){
      M[i,(i+1):d] <- pmax(coeff/(1:(d-i)), min.coeff)
    }
    M <- diag(d) + M + t(M)
    if(!is.positive.definite(sin(pi*M/2))) print("wowowo mister")
    return(M)
  }
  
  if(type == "un3"){
    M <- matrix(0,d,d)
    for(i in 1:(d-1)){
      M[i,(i+1):d] <- pmax(coeff - .05*(0:(d-i-1)), min.coeff)
    }
    M[t(combn(d,2))] <- M[t(combn(d,2))]
    M <- diag(d) + M + t(M)
    if(!is.positive.definite(sin(pi*M/2))) print("wowowo mister")
    return(M)
  }
}

nestMartix <- function(M, toNest){
  ncols <- sapply(toNest, ncol)
  ncols <- c(ncols,rep(1,ncol(M) - length(toNest)))
  
  B <- matrix(0,sum(ncols),ncol(M))
  

  
  ss <- c(0,cumsum(ncols))
  ind <- do.call("rbind",lapply(1:length(ncols), function(k){cbind((ss[k]+1):ss[k+1],rep(k,ncols[k]))}))
  B[ind] <- 1
  
  M <- (B %*% M %*% t(B))
  
  for(k in 1:length(toNest)){
    M[(ss[k]+1):ss[k+1],(ss[k]+1):ss[k+1]] <- toNest[[k]]
  }
  
  if(!is.positive.definite(sin(pi*M/2))) print("wowowo mister")
  M
}
