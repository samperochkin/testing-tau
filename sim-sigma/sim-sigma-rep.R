
# setup -------------------------------------------------------------------

require(pcaPP)
require(Matrix)
require(parallel)

source("functions/computeTh.R")
source("functions/buildSigma.R")

n <- 150



# functions ---------------------------------------------------------------

jackknifeVariance <- function(X){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))
  
  Tau.hat <- pcaPP::cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  
  ij.mat <- t(combn(d,2))
  
  # cl <- makeCluster(detectCores()-1)
  # clusterExport(cl, c("X","n","ij.mat","Tau.hat"), envir = environment())
  
  # Ths <- parLapply(cl, 1:n, function(r){
  Ths <- lapply(1:n, function(r){
    Y <- X[r,] < t(X[-r,])
    Th <- 2*Reduce("+",lapply(1:ncol(Y), function(r) (outer(Y[,r],Y[,r],"-"))==0)) - (n-1)
    (Tau.hat*(n*(n-1))/2 - Th)/((n-1)*(n-2)/2) 
  })
  # stopCluster(cl)
  
  Tcs <- sapply(Ths, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(Ths, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)
  
  s2 <- mean(apply(Ths, 1, var))*(n-1)^2
  s1 <- mean(apply(Tcs, 1, var))*(n-1)^2
  s0 <- var(Tbs)*(n-1)^2
  
  c(s2,s1,s0)
}


leave2Variance <- function(X){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))
  rs.mat <- t(combn(n,2)) 
  
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  
  ij.mat <- t(combn(d,2))
  
  # cl <- makeCluster(detectCores()-1)
  # clusterExport(cl, c("X","n","ij.mat","rs.mat","Tau.hat"), envir = environment())
  
  # Ths <- parLapply(cl, 1:nrow(rs.mat), function(k){
  Ths <- lapply(1:nrow(rs.mat), function(k){
    pcaPP::cor.fk(X[-rs.mat[k,],])
  })
  # stopCluster(cl)
  
  Tcs <- sapply(Ths, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(Ths, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)
  
  s2 <- mean(apply(Ths - t.hat, 1, crossprod))*(n-2)^2/(2*nrow(rs.mat))
  s1 <- mean(apply(Tcs - t.col, 1, crossprod))*(n-2)^2/(2*nrow(rs.mat))
  s0 <- crossprod(Tbs-t.bar)*(n-2)^2/(2*nrow(rs.mat))
  
  c(s2,s1,s0)
}


pluginVariance <- function(X){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))
  ij.l.mat <- matrix(0,d,d)
  ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p
  
  
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.bar <- mean(t.hat)
  
  Th <- computeTh(X)
  Shh <- buildSigma(Th,t.hat,n,F)
  Shb <- buildSigma(Th,rep(t.bar,p),n,F)
  
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)

  sapply(2:0, function(i){
    c(mean(Shh[which(BtB == i)]),
      mean(Shb[which(BtB == i)]))
  })
}



# replicates --------------------------------------------------------------

M <- 500

cl <- makeCluster(c(rep(c("dms1","dms2","dms3"),5),
                    rep(c("dms5","dms6","dms7"),5),
                    rep(c("dms9","dms10"),5),
                    rep("dms11",12)))
clusterExport(cl, c("jackknifeVariance","leave2Variance","pluginVariance",
                    "computeTh","buildSigma","n"), envir = environment())
clusterEvalQ(cl, {
  require(pcaPP)
  require(Matrix)
  require(parallel)
})

res <- parLapply(cl, 1:M, function(dummy){
  ds <- c(1000)
  d <- max(ds)
  p <- d*(d-1)/2
  
  X <- rnorm(n) + matrix(rnorm(n*d),n,d)
  ds <- c(5,10,25)
  
  sigma.JK <- sapply(ds, function(d){
    print(d)
    jackknifeVariance(X[,1:d])
  })
  
  dmax <- 50
  sigma.L2 <- sapply(ds[ds <= dmax], function(d){
    print(d)
    leave2Variance(X[,1:d])
  })
  
  sigma.PI <- sapply(ds[ds <= dmax], function(d){
    print(d)
    t(n*pluginVariance(X[,1:d]))
  })
  
  
  d <- ncol(X)
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  t.cen <- t.hat - t.bar
  
  # Monte Carlo
  s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))

  
  ss2.JK <- (c(1,-2,1) %*% sigma.JK)/c(s210)
  ss2.L2 <- (c(1,-2,1) %*% sigma.L2)/c(s210)
  ss2.PI1 <- (c(1,-2,1) %*% sigma.PI[1:3,])/c(s210)
  ss2.PI2 <- (c(1,-2,1) %*% sigma.PI[4:6,])/c(s210)
  
  ss1.JK <- (c(0,1,-1) %*% sigma.JK)/c(s10)
  ss1.L2 <- (c(0,1,-1) %*% sigma.L2)/c(s10)
  ss1.PI1 <- (c(0,1,-1) %*% sigma.PI[1:3,])/c(s10)
  ss1.PI2 <- (c(0,1,-1) %*% sigma.PI[4:6,])/c(s10)
  
  
  v <- c(
    ss2.JK,ss2.L2,ss2.PI1,ss2.PI2,
    ss1.JK,ss1.L2,ss1.PI1,ss1.PI2
  )
  v <- matrix(v,3,8)
  colnames(v) <- c("s2JK","s2L2","s2PI1","s2PI2","s1JK","s1L2","s1PI1","s1PI2")
  v
})

stopCluster(cl)

saveRDS(res,paste0("res",n,".rds"))