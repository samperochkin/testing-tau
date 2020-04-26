library(mvtnorm)
library(data.table)


source("functionsLow/generateData.R")
X <- generateData(n=150, d=5, tau=.15, dtau=0, dtau_type = "none", distribution = "normal")
M <- 5000

sapply(list.files("functionsLowBoot/", full.names = T), source, local=environment())
# setup -------------------------------------------------------------------
n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
l.mat <- matrix(0,d,d)
l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p

Tau.hat <- pcaPP::cor.fk(X)
tau.hat <- Tau.hat[ij.mat]
SPs <- computeSigmaPlugin(X, ij.mat,tau.hat,T,l.mat)

HP <- computeHajekProjection(X,ij.mat)
SJs <- computeSigmaJackknife(HP,ij.mat,T,l.mat)

# B <- rep(1,p)
# IBB <- diag(p) - matrix(1/p,p,p)
# Sigma.hats <- list(ShP = ShP, ShJ = ShJ)
# 
# SbJ <- SJs$SbJ
# 
# eig <- eigen(SbJ)
# SbJi <- eig$vectors %*% diag(1/eig$values) %*% t(eig$vectors)
# SbJi2 <- eig$vectors %*% diag(1/sqrt(eig$values)) %*% t(eig$vectors)
# 
# SbJi2 %*% (IBB) %*% SJs$SbJ %*% (IBB) %*% SbJi2 - IBB
# B %*% solve(t(B) %*% SbJi %*% B) %*% t(B) %*% SbJi - B %*% solve(t(B) %*% B) %*% t(B)


