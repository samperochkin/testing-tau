library(parallel)
library(data.table)
library(mvtnorm)
library(pcaPP)


source("functions/generateData.R")

clus <- makeCluster(detectCores()-1) 
clusterEvalQ(clus, expr={
  library(data.table)
  library(mvtnorm)
  library(pcaPP)
})

clusterExport(clus, varlist=c("generateData"),envir = environment())


pvals <- parSapply(clus, 1:200, function(dummy){
  
  n <- 100
  d <- 20
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
  t.bar <- mean(t.hat)
  t.cen <- t.hat-t.bar
  
  # components of Hajek projection
  # T.hajek <- lapply(1:n, function(r){
  #   v <- t(X[r,] < t(X[-r,]))
  #   v <- rowSums(apply(v, 1, function(v){
  #     4*(outer(v,v,"=="))[ij.mat]
  #   }))
  #   V <- diag(d)
  #   V[ij.mat] <- V[ij.mat[,2:1]] <- v #- mean(v)# - t.cen/n
  #   V
  # })
  # 
  # Ths <- sapply(T.hajek, function(Th) Th[ij.mat])
  # Tbs <- apply(Ths, 2, function(th) rep(mean(th),p))
  # 
  # BOOTsup <- replicate(2000, {
  #   sqrt(n)*max(abs((Ths - Tbs)/(n*(n-1)) - t.cen/n) %*% rnorm(n))
  # })

  # C.gen <- sapply(1:n, function(r){
  #   V <- t(X[r,] < t(X[-r,]))
  #   V <- rowSums(apply(V, 1, function(v){
  #     4*(outer(v,v,"=="))[ij.mat]
  #   }))/(n*(n-1))
  #   V - mean(V) - t.cen/n
  # })
  
  C.gen <- lapply(1:n, function(r){
    V <- t(X[r,] < t(X[-r,]))
    V <- Reduce("+",lapply(1:nrow(V), function(k){
      4*(outer(V[k,],V[k,],"=="))
    }))
    V
  })
  Ths <- sapply(C.gen, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)

  
  # tiime <- Sys.time()
  # BOOTsup <- replicate(2000, {
  #   sqrt(n)*max(abs((C.gen/(n*(n-1)) - t.cen/n) %*% rnorm(n)))
  # })
  BOOTsup <- replicate(2000, {
    sqrt(n)*max(abs(((Ths-t(matrix(Tbs,n,p)))/(n*(n-1)) - t.cen/n) %*% rnorm(n)))
  })
  
  
  mean(BOOTsup > sqrt(n)*max(abs(t.cen)))
})
stopCluster(clus)

plot(seq_along(pvals)/length(pvals),sort(pvals),ylim=c(0,1))

mean(pvals < .05)
hist(pvals)
plot(seq_along(pvals)/length(pvals),sort(pvals))
abline(a=0,b=1,col="red")
