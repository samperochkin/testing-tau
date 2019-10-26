library(parallel)
library(data.table)
library(mvtnorm)
library(pcaPP)


source("functions/generateData.R")

clus <- makeCluster(c(rep("dms11",10),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7","dms9"),4))) 
clusterEvalQ(clus, expr={
  library(data.table)
  library(mvtnorm)
  library(pcaPP)
})

clusterExport(clus, varlist=c("generateData",
                              "testEquiRankJackknife"),envir = environment())


pvals <- parSapply(clus, 1:2000, function(dummy){
  n <- 100
  d <- 10
  tau <- .3
  dtau <- 0
  dtau_type <- "none"
  distribution <- "normal"
  
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))

  X <- generateData(n,d,tau,dtau,dtau_type,distribution)
  

  
  # computation of estimates ------------------------------------------------
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  t.star <- (d-1)/(d-2)*(t.col[ij.mat[,1]] + t.col[ij.mat[,2]]) - d/(d-2)*t.bar
  t.cen <- t.hat-t.bar
  
  # components of Hajek projection
  T.hajek <- lapply(1:n, function(r){
    v <- t(X[r,] < t(X[-r,]))
    v <- rowSums(apply(v, 1, function(v){
      4*(outer(v,v,"=="))[ij.mat]
    }))
    V <- diag(d)
    V[ij.mat] <- V[ij.mat[,2:1]] <- v #- mean(v)# - t.cen/n
    V
  })
  
  Tcs <- sapply(T.hajek, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(T.hajek, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, function(th) rep(mean(th),p))
  
  BOOTsup <- replicate(2000, {
    sqrt(n)*max(abs((Ths - Tbs)/(n*(n-1)) - t.cen/n) %*% rnorm(n))
  })
  
  
  mean(BOOTsup > sqrt(n)*max(abs(t.cen)))
})
stopCluster(clus)

saveRDS(pvals, "pvals_extra_3.rds")

