library(MASS)
library(pcaPP)
library(mvtnorm)

n <- 100
d <- 5
p <- d*(d-1)/2

X <- rmvnorm(n,rep(0,d),diag(d)*.4 + .6)

ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p


# computation of estimates ------------------------------------------------
Tau.hat <- cor.fk(X)
t.hat <- Tau.hat[ij.mat]
image(t(Tau.hat[d:1,]), zlim = c(0,1))

# B <- matrix(1,p,1)
B <- matrix(0,p,4)# + runif(p*ncol(B),-1,1)
B[1:5,1] <- 1
B[6:10,2] <- 1
# B[11:p-1,3] <- 1
B[11:15,3] <- 1
B[c(1,2,7,8),3] <- 1
B[3,3] <- -1
B[11:14,4] <- -1
# B[p,4] <- 1
B[16:p,4] <- 1

B <- B/rowSums(abs(B))

# B <- cbind(matrix(1,p,1),c(0,1),c(0,0,1,1))
# B <- matrix(1:5,p,5,byrow=T)
# B <- B*matrix(runif(p*5,0,1),p,5)
# 
# B[1:3,] <- 0
# B[1,1] <- 100
# B[2,2] <- 1
# B[3,] <- rep(1,ncol(B))
# 
# B <- B/max(colSums(abs(B)))
# B[B < .01] <- .01


# B <- matrix(runif(p*3,-1,1),p,3)
# B[3,1] <- 0
# B[4,] <- rep(2,ncol(B))
# B[5,] <- rep(.001,ncol(B))

# B[4,] <- c(100,0,0)
# B[-(1:10),1] <- 0
# B[-(8:30),2] <- 0
# B[-sample(p,5),3] <- 0


I <- diag(p)
BBp <- B %*% ginv(B)
IBB <- I-BBp

t.hat <- Tau.hat[ij.mat]
t.bar <- BBp %*% t.hat

source("sim-main/functions/computeTh.R")
source("sim-main/functions/buildSigma.R")
Th <- computeTh(X)
# Sh <- n*buildSigma(Th,rep(t.bar,p),n,F)
Sh <- n*buildSigma(Th,t.hat,n,F)
Shi <- solve(Sh)

eig <- eigen(Sh)
eig$values

Shh <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
Shih <- solve(Shh)


G <- B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shi
IG <- I - G

CCp <- Shih %*% G %*% Shh
SS <- I - CCp
sum(abs(SS - t(SS)))
SS <- (t(SS) + SS)/2
# 
# 
eiB <- eigen(I - BBp)
eiC <- eigen(SS)
# 
U <- eiB$vectors[,1:(p-ncol(B)),drop=F]
V <- eiC$vectors[,1:(p-ncol(B)),drop=F]
# U <- eiB$vectors[,]
# # V <- eiC$vectors[,]
# 
Ui <- t(U)
Vi <- t(V)
# 
# # U %*% Ui - (I-BBp)
# # Ui %*% U
# # V %*% Vi - SS
# # Vi %*% V


any(rowSums(abs(I-BBp)) > sqrt(p))
any(rowSums((I-BBp)^2) > 1)
max(rowSums((I-BBp)^2))
hist(rowSums(abs(I-BBp)))
abline(v=4,col="red")

max(rowSums(abs(I-BBp)))
which.max(rowSums(abs(I-BBp)))
hist(rowSums(abs(I-BBp)))
abline(v=4,col="red")

# any(rowSums(I-G) > 1)
# any(rowSums(I-SS) > 1)


# hist(BBp %*% t.hat)
# plot(rowSums(abs(U)))
# range(rowSums(abs(U)))
# sqrt(p-ncol(B))


D <- diag(BBp %*% Sh %*% (2*Sh - BBp))
plot(D)
abline(h=0, col="red")

min(D)

DD <- diag(Sh)
range(DD)
range(D)

plot(DD,D,xlim=range(c(D,DD)), ylim=range(c(D,DD)))
abline(h=0, col="red")

matrixcalc::is.positive.definite(Sh)
all(D > 0)
min(diag(IBB %*% Sh %*% IBB))<min(diag(Ui %*% Sh %*% U))
min(diag(IBB %*% Sh %*% IBB))
min(diag(Ui %*% Sh %*% U))

min(diag(SS %*% Sh %*% SS))
min(diag(Vi %*% Sh %*% V))




range(rowSums(U))
range(rowSums(IBB))
range(rowSums(abs(U)))
range(rowSums(abs(IBB)))
# range(colSums(abs(SS)))




eiBB <- eigen(t(B) %*% B)

t(B) %*% B
solve(t(B) %*% B)
W <- eiBB$vectors
eiBB$values
