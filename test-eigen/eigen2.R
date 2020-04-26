setwd("sim-main")

library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)
source("functions/computeTh.R")
source("functions/buildSigma.R")
source("functions/generateData.R")
source("functions/testEquiRankPlugin.R")

n=100
d=6
k=NULL
tau=.5
dtau=0
distribution="normal"
dtau_type <- "none"


X <- generateData(n,d,tau,dtau,dtau_type,distribution)

# d=3

p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p


# computation of estimates ------------------------------------------------
Tau.hat <- cor.fk(X[,1:d])
t.bar <- mean(Tau.hat[ij.mat])


# computation of estimates: orthogonal components
B <- Matrix(0, nrow = p, ncol = d, sparse = T)
for(i in 1:d){
  B[ij.l.mat[i,-i],i] <- 1
}
BtB <- B %*% t(B)
G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B))


#### Plugin tests
Th <- computeTh(X[,1:d])
Sh <- n*buildSigma(Th,rep(t.bar,p),n,F)
# Sh <- diag(p)

for(i in 1:3){
  Sh[which(BtB == i-1)] <- mean(Sh[which(BtB == i-1)])
  # Sh[which(BtB == i-1)] <- sigma[i]
}
Shi <- solve(Sh)

sigma <- rev(Sh[1,c(1,2,p)])
sigmaI <- rev(Shi[1,c(1,2,p)])


deltaFun <- function(s){
  s0 <- s[1]
  s1 <- s[2]
  s2 <- s[3]
  
  c(
    s2 + 2*(d-2)*s1 + (p - 2*d + 3)*s0,
    s2 + (d-4)*s1 - (d-3)*s0,
    s2 - 2*s1 + s0
  )
}

delta <- deltaFun(sigma)
delta
table(round(sort(eigen(Sh)$value, decreasing=T),5))

deltaI <- deltaFun(sigmaI)
deltaI
table(round(sort(eigen(Shi)$value, decreasing=T),5))

1/delta
deltaI




s <- sigma
s[2] <- -7/2*s[1]

deltaFun(s)


for(k in 1:10){
  j <- ceiling((1 + sqrt(1+8*k))/2)
  i <- k - (j-1)*(j-2)/2
  
  print(c(i,j))
}


