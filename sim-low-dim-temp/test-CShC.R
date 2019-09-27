
library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)

source("functions/computeTh2.R")
source("functions/computeThChunks.R")

source("functions/computeTh.R")
source("functions/buildSigma.R")
source("functions/generateData.R")
source("functions/testEquiRank.R")

r <- 1
n <- sim.grid[r,]$n
d <- sim.grid[r,]$d
tau <- sim.grid[r,]$tau
dtau <- sim.grid[r,]$dtau
dtau_type <- sim.grid[r,]$dtau_type
distribution <- sim.grid[r,]$distribution
X <- generateData(n,d,tau,dtau,dtau_type,distribution)



s <- computeTh2(X,5)
S <- computeTh(X)


ij.mat <- t(combn(d,2))
r.mat <- matrix(0,d,d)
r.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:nrow(ij.mat)
B <- Matrix(0, nrow = nrow(ij.mat), ncol = d, sparse = T)
for(i in 1:d){
  B[r.mat[i,-i],i] <- 1
}
BtB <- B %*% t(B)

s2 <- c(mean(diag(S)),
  mean(S[which(BtB == 1)]),
  mean(S[which(BtB == 0)]))


s/s2
(s-s2)


