library(HAC)
library(mvtnorm)
library(pcaPP)
library(Matrix)

source("functions/generateData.R")
source("functions/buildSigma.R")
source("functions/computeTh.R")
source("functions/computeTh4.R")
source("functions/computeTh5.R")

X <- generateData(n = 100,
                  d = 50,
                  tau = .3,
                  dtau = 0,
                  distribution = "joe")

n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2
l.ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p

Tau.hat <- cor.fk(X)
tau.hat <- Tau.hat[l.ij.mat]
tau.bar <- mean(tau.hat)

Th <- computeTh(X)
B <- Matrix(0, nrow = p, ncol = d, sparse = T)
for(i in 1:d){
  B[ij.l.mat[i,-i],i] <- 1
}
BtB <- B %*% t(B)
for(i in 1:3){
  Th[which(BtB == i-1)] <- mean(Th[which(BtB == i-1)])
}

dd <- 5
K <- 1000

theta <- Th[1,c(1,2,p)]
theta.a <- computeTh3(X, dd, K)
theta.b <- computeTh4(X, dd, K)

cbind(theta,theta.a,theta.b)


sigma <- theta - 2*(2*n - 3) / (n*(n - 1)) * (tau.bar + 1)^2
sigma.a <- theta.a - 2*(2*n - 3) / (n*(n - 1)) * (tau.bar + 1)^2
sigma.b <- theta.b - 2*(2*n - 3) / (n*(n - 1)) * (tau.bar + 1)^2

cbind(sigma,sigma.a,sigma.b)





delta2 <- sigma[1] + (d-4)*sigma[2] - (d-3)*sigma[3]
delta3 <- sigma[1] - 2*sigma[2] + sigma[3]
delta <- c(delta2,delta3)


delta2.a <- sigma.a[1] + (d-4)*sigma.a[2] - (d-3)*sigma.a[3]
delta3.a <- sigma.a[1] - 2*sigma.a[2] + sigma.a[3]
delta.a <- c(delta2.a,delta3.a)


delta2.b <- sigma.b[1] + (d-4)*sigma.b[2] - (d-3)*sigma.b[3]
delta3.b <- sigma.b[1] - 2*sigma.b[2] + sigma.b[3]
delta.b <- c(delta2.b,delta3.b)


cbind(delta,delta.a,delta.b)
1/cbind(delta,delta.a,delta.b)
cbind(1/delta*c(d-1,p-d),1/delta.a*c(d-1,p-d),1/delta.b*c(d-1,p-d))

crossprod(tau.hat)

M <- 1000

Z <- replicate(M, {
  Z0 <- rnorm(1,0,sqrt(sigma[3]))
  Z1 <- rnorm(d,0,sqrt(sigma[2]-sigma[3]))
  Z2 <- rnorm(p,0,sqrt(sigma[1]-2*sigma[2]+sigma[3]))
  
  sqrt(n)*(Z0 + Z1[l.ij.mat[,1]] + Z1[l.ij.mat[,2]] + Z2)
})

dim(Z)

hist(apply(Z,2,max))
abline(v=sqrt(n)*max(tau.hat-tau.bar), col="blue")


Z.a <- replicate(M, {
  Z0 <- rnorm(1,0,sqrt(sigma.a[3]))
  Z1 <- rnorm(d,0,sqrt(sigma.a[2]-sigma.a[3]))
  Z2 <- rnorm(p,0,sqrt(sigma.a[1]-2*sigma.a[2]+sigma.a[3]))
  
  sqrt(n)*(Z0 + Z1[l.ij.mat[,1]] + Z1[l.ij.mat[,2]] + Z2)
})

hist(apply(Z.a,2,max))
abline(v=sqrt(n)*max(tau.hat-tau.bar), col="red")

mean(Z > max(tau.hat-tau.bar))
mean(Z.a > max(tau.hat-tau.bar))



jack <- sapply(1:n, function(r){
  th <- cor.fk(X[-r,])
  
  tb <- (sum(th)-d)/(2*p)
  tbb <- (colSums(th) - 1)/(d-1)
  
  c(tb-tau.bar,tbb-tb)
})

S <- (n-1)/n*tcrossprod(jack - rowMeans(jack))


S <- (n-1)*cov(jack)/n
hist(S[upper.tri(S)], breaks=20)
diag(S)

sigma

s0 <- S[1,1]
# s1 <- mean(diag(S[-1,-1]))
s1 <- mean(diag(S[-1,-1]))
              
c(s1,s0)/2



tb <- (colSums(Tau.hat)-1)/(d-1)
var(tau.hat - (tb[l.ij.mat[,1]]+tb[l.ij.mat[,2]])/2)
sigma[1]-2*sigma[2]+sigma[3]


G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B))
tb <- G %*% tau.hat

sigma[1] - 2*sigma[2] + sigma[3]
s2 <- crossprod(tau.hat - tb)/(p-d)
s2

sigma[2]-sigma[3]
s1 <- crossprod(tb-tau.bar)/(2*p)
s1

sigma[1]-sigma[3]
sigma[2]-sigma[3]
ss <- c(s2+2*s1,s1)

Sh <- Th - 2*(2*n - 3) / (n*(n - 1)) * (tau.bar + 1)^2
Shh <- Matrix(data=0,nrow=p,ncol=p)
for(i in 1:2){
  Shh[which(BtB == 3-i)] <- ss[i]
}
# Shh <- Shh + sigma[3]


mahalanobis(tau.hat,rep(tau.bar,p),Sh)
mahalanobis(tau.hat,rep(tau.bar,p),Shh)
