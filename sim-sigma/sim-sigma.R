require(pcaPP)
require(parallel)
n <- 100
ds <- c(1000)
d <- max(ds)
p <- d*(d-1)/2

X <- rnorm(n) + matrix(rnorm(n*d),n,d)
ds <- c(5,10,25,50,100,250)



# Jackknife ---------------------------------------------------------------

jackknifeVariance <- function(X){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))
  
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  
  ij.mat <- t(combn(d,2))
  
  cl <- makeCluster(detectCores()-1)
  clusterExport(cl, c("X","n","ij.mat","Tau.hat"), envir = environment())
  
  Ths <- parLapply(cl, 1:n, function(r){
    Y <- X[r,] < t(X[-r,])
    Th <- 2*Reduce("+",lapply(1:ncol(Y), function(r) (outer(Y[,r],Y[,r],"-"))==0)) - (n-1)
    (Tau.hat*(n*(n-1))/2 - Th)/((n-1)*(n-2)/2) 
  })
  stopCluster(cl)
  
  Tcs <- sapply(Ths, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(Ths, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)
  
  s2 <- mean(apply(Ths, 1, var))*(n-1)^2
  s1 <- mean(apply(Tcs, 1, var))*(n-1)^2
  s0 <- var(Tbs)*(n-1)^2

  c(s2,s1,s0)
}


jackknifeVariance <- function(X){
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  ij.mat <- t(combn(d,2))
  
  Tau.hat <- cor.fk(X)
  t.hat <- Tau.hat[ij.mat]
  t.col <- (colSums(Tau.hat)-1)/(d-1)
  t.bar <- mean(t.col)
  
  ij.mat <- t(combn(d,2))
  
  cl <- makeCluster(detectCores()-1)
  clusterExport(cl, c("X","n","ij.mat","Tau.hat"), envir = environment())
  
  Ths <- parLapply(cl, 1:(5*n), function(r){
    ind <- sample(n,3)
    pcaPP::cor.fk(X[-ind,])
  })
  stopCluster(cl)
  
  Tcs <- sapply(Ths, function(Th) (colSums(Th)-1)/(d-1))
  Ths <- sapply(Ths, function(Th) Th[ij.mat])
  Tbs <- apply(Ths, 2, mean)
  
  s2 <- mean(apply(Ths - t.hat, 1, crossprod))*(n-3)^2/(3*5*n)
  s1 <- mean(apply(Tcs - t.col, 1, crossprod))*(n-3)^2/(3*5*n)
  s0 <- crossprod(Tbs-t.bar)*(n-3)^2/(3*5*n)
  
  c(s2,s1,s0)
}


sigma.JK <- sapply(ds, function(d){
  print(d)
  jackknifeVariance(X[,1:d])
})

plot(ds, sigma.JK[1,], ylim = range(c(sigma.JK)), type="l", col=2)
lines(ds, sigma.JK[2,], col=3)
lines(ds, sigma.JK[3,], col=4)

abline(h=sigma.JK[,length(ds)], lty=3)



# Plug-in -----------------------------------------------------------------

source("sim-low-dim/functions/computeTh.R")
source("sim-low-dim/functions/buildSigma.R")
require(Matrix)

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
  
  Sh <- buildSigma(computeTh(X),t.bar,n)
  
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  BtB <- B %*% t(B)
  G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B))
  
  sapply(2:0, function(i){
    mean(Sh[which(BtB == i)])
  })
}

dmax <- 50
sigma.PI <- sapply(ds[ds <= dmax], function(d){
  print(d)
  pluginVariance(X[,1:d])
})

lines(ds[ds <= dmax], n*sigma.PI[1,], col=2, lty=2)
lines(ds[ds <= dmax], n*sigma.PI[2,], col=3, lty=2)
lines(ds[ds <= dmax], n*sigma.PI[3,], col=4, lty=2)



# Direct ------------------------------------------------------------------

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
s21 <- s210 + s10
ss <- c(s21,s10)

ss.JK <- (sigma.JK[1:2,]-sigma.JK[2:3,])
ss.PI <- n*(sigma.PI[1:2,]-sigma.PI[2:3,])


plot(ds, ss.JK[1,], ylim = range(c(ss,ss.JK,ss.PI)), type="l", col=2)
lines(ds, ss.JK[2,], col=3)

lines(ds[ds <= dmax], ss.PI[1,], col=2, lty=2)
lines(ds[ds <= dmax], ss.PI[2,], col=3, lty=2)


abline(h=ss, lty=3)
