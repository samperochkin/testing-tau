
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
source("functions/createGrid.R")

n = 100
d = 20
tau = .3
dtau = 0
distribution = "joe"
num_sim = 1
library(data.table)
sim.grid <- createGrid(n, d, tau, dtau, distribution, 1)


r <- 1
n <- sim.grid[r,]$n
d <- sim.grid[r,]$d
tau <- sim.grid[r,]$tau
dtau <- sim.grid[r,]$dtau
dtau_type <- sim.grid[r,]$dtau_type
distribution <- sim.grid[r,]$distribution

d <- 30
p <- d*(d-1)/2
l.ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p

X <- generateData(n,d,tau,dtau,dtau_type,distribution)



Tau.hat <- cor.fk(X)

# tt <- Sys.time()
theta <- computeTh2(X,5)
# difftime(Sys.time(),tt)
# tt <- Sys.time()
# theta <- computeTh2(X,2)
# difftime(Sys.time(),tt)


sigma <- theta - 2*(2*n - 3) / (n*(n - 1)) * (mean(Tau.hat[t(combn(d,2))]) + 1)^2
sigma2 <- sigma[1]
sigma1 <- sigma[2]
sigma0 <- sigma[3]

delta2 <- sigma2 + (d-4)*sigma1 - (d-3)*sigma0
delta3 <- sigma2 - 2*sigma1 + sigma0
delta <- c(delta2,delta3)

B <- Matrix(0, nrow = p, ncol = d, sparse = T)
for(i in 1:d){
  B[ij.l.mat[i,-i],i] <- 1
}
BtB <- B %*% t(B)

# G.sparse <- Matrix(0,p,p)
# G.sparse[which(BtB == 2)]
# G.sparse[which(BtB == 1)] 

g2 <- 2/(d-1)
g1 <- (d-3)/((d-1)*(d-2))
g0 <- -2/((d-1)*(d-2))

th <- cor.fk(X)[l.ij.mat]
th.star <- as.vector(g2*th + g1*crossprod((BtB == 1), th) + g0*crossprod((BtB == 0), th))

hist(th.star)

maha <- crossprod(th-th.star)/delta3 + crossprod(th.star-mean(th))/delta2
pchisq(maha, df=p-1)





# Sh <- buildSigma(computeTh(X),rep(mean(th),p),n,F)
# for(i in 1:3){
#   Sh[which(BtB == i-1)] <- mean(Sh[which(BtB == i-1)])
#   # Sh[which(BtB == i-1)] <- sigma[4-i]
# }
# G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B))
# 
# Sh[1,c(1,2,p)]
# sigma
# 
# mahalanobis(th-mean(th),F,Sh)
# mahalanobis(c((diag(p)-G)%*%th),F,Sh)
# mahalanobis(c((G-1/p)%*%th),F,Sh)
# 
