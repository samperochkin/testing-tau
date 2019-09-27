dd <- 1000
pp <- dd*(dd-1)/2

n <- 100
X <- rnorm(n) + matrix(rnorm(n*dd),n,dd)

ds <- c(5,10)
# ds <- c(5,10,25)

M <- 200

sigma.JK <- sapply(ds, function(d){
  print(d)
  rowMeans(replicate(M, {
    jackknifeVariance(X[,sample(dd,d)])
  }))
})

dmax <- 50
sigma.L2 <- sapply(ds[ds <= dmax], function(d){
  print(d)
  rowMeans(replicate(M, {
    leave2Variance(X[,sample(dd,d)])
  }))
})  



sigma.PI <- sapply(ds[ds <= dmax], function(d){
  print(d)
  rowMeans(replicate(M, {
    t(n*pluginVariance(X[,sample(dd,d)]))
  }))
})


ij.mat <- t(combn(dd,2))
Tau.hat <- cor.fk(X)
t.hat <- Tau.hat[ij.mat]
t.col <- (colSums(Tau.hat)-1)/(dd-1)
t.bar <- mean(t.col)
t.cen <- t.hat - t.bar

# Monte Carlo
s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(pp-dd)
s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(dd*(dd-1))


ss2.JK <- (c(1,-2,1) %*% sigma.JK)/c(s210)
ss2.L2 <- (c(1,-2,1) %*% sigma.L2)/c(s210)
ss2.PI <- (c(1,-2,1) %*% sigma.PI)/c(s210)

ss1.JK <- (c(0,1,-1) %*% sigma.JK)/c(s10)
ss1.L2 <- (c(0,1,-1) %*% sigma.L2)/c(s10)
ss1.PI <- (c(0,1,-1) %*% sigma.PI)/c(s10)


v <- c(
  ss2.JK,ss2.L2,ss2.PI,
  ss1.JK,ss1.L2,ss1.PI
)
v <- matrix(v,length(ds),6)
colnames(v) <- c("s2JK","s2L2","s2PI1","s1JK","s1L2","s1PI1")
v
