
library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)

source("functions/computeTh3.R")
source("functions/computeThChunks.R")

source("functions/computeTh.R")
source("functions/buildSigma.R")
source("functions/generateData.R")
source("functions/testEquiRank.R")
source("functions/createGrid.R")

n = 150
d = 5
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

n <- 150
d <- 30
p <- d*(d-1)/2
l.ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p

tiime <- Sys.time()
res <- replicate(1000,{
  
  # 
  # print("hey")
  X <- generateData(n,d,tau,dtau,dtau_type,distribution)
  
  
  
  Tau.hat <- cor.fk(X)
  th <- Tau.hat[l.ij.mat]
  
  # tt <- Sys.time()
  # theta <- computeTh2(X,5)
  theta <- computeTh3(X,10,50)
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
  
  th.star <- as.vector(g2*th + g1*Matrix::crossprod((BtB == 1), th) + g0*Matrix::crossprod((BtB == 0), th))
  
  # hist(th.star)
  
  loss <- crossprod(th-th.star)/delta3 + crossprod(th.star-mean(th))/delta2
  pval <- pchisq(loss, df=p-1, lower.tail = T)
  
  c(loss,pval)
})
tiime <- difftime(Sys.time(),tiime)

# res

hist(res[1,], breaks=25, probability = T)
xx <- seq(p-5*d,p+5*d)
lines(xx,dchisq(xx,p-1))

hist(res[2,], breaks=25)


Th <- computeTh(X)
for(i in 1:3){
  Th[which(BtB == i-1)] <- mean(Th[which(BtB == i-1)])
}
Th[1,c(1,2,p)]
theta

loss


quantile(res[2,],seq(0,1,.05))
