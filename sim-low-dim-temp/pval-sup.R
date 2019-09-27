# basic sample
mc_samp <- lapply(seq(5,20,5), function(d){
  replicate(10000, {
    p <- d*(d-1)/2
    l.ij.mat <- t(combn(d,2))
    t0 <- rnorm(1)
    t1 <- rnorm(d)
    t2 <- rnorm(p)
    cbind(rep(t0,p),t1[l.ij.mat[,1]]+t1[l.ij.mat[,2]],t2)
  })
})
names(mc_samp) <- seq(5,20,5)


# r <- which(res$loss_type %in% c("sup","sup2","sup3") & res$dtau == 0)[1]
# Compute Monte Carlo quantile
monteCarlo <- function(res,r){
  loss_type <- res[r,loss_type]
  loss <- res[r,loss]
  n <- res[r,n]
  d <- res[r,d]
  p <- res[r,p]
  sigma2 <- n*res[r,sigma2]
  sigma1 <- n*res[r,sigma1]
  sigma0 <- n*res[r,sigma0]
  l.ij.mat <- t(combn(d,2))
  ij.l.mat <- matrix(NA,d,d)
  ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p
  
  if(sigma0 < 0) sigma0 <- 0
  
  if(loss_type == "sup"){
    G <- diag(p) - 1/p
  }else if(loss_type == "sup2"){
    B <- Matrix(0, nrow = p, ncol = d, sparse = T)
    for(i in 1:d){
      B[ij.l.mat[i,-i],i] <- 1
    }
    BtB <- B %*% t(B)
    G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B)) - 1/p
  }else if(loss_type == "sup3"){
    B <- Matrix(0, nrow = p, ncol = d, sparse = T)
    for(i in 1:d){
      B[ij.l.mat[i,-i],i] <- 1
    }
    BtB <- B %*% t(B)
    G <- diag(p) - as.matrix(B %*% solve(crossprod(B)) %*% t(B))
  }
  
  
  mc_samp_temp <- apply(mc_samp[[as.character(d)]], 3, function(M){
    max(abs(G %*% M %*% sqrt(c(sigma0,
                 sigma1 - sigma0,
                 sigma2 - 2*sigma1 + sigma0))))
  })
  mean(mc_samp_temp > loss)
}


clus <- makeCluster(detectCores()/2)
clusterEvalQ(clus, expr={
  library(data.table)
  library(Matrix)
})
clusterExport(clus, varlist=c("monteCarlo","res","mc_samp"), envir = environment())
# 
# 
ind <- which(res$loss_type %in% c("sup","sup2","sup3"))
res[ind, pval := parSapply(clus,ind, function(r) monteCarlo(res,r))]
# res[ind, pval := sapply(ind, function(r) monteCarlo(res,r))]

stopCluster(clus)
