library(mvtnorm)
sapply(list.files("sim-main/functionsHigh/", full.names = T), source, local=environment())
source("sim-main/functionsLow2/computeSigmaJackknife.R")
source("sim-main/functionsLow2/averageSigma.R")

M <- 5000
X <- generateData(n=200, d=15, tau=.2, dtau=0, dtau_type = "none", distribution = "normal")

n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
l.mat <- matrix(0,d,d)
l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p

Tau.hat <- pcaPP::cor.fk(X)
tau.hat <- Tau.hat[ij.mat]
tau.col <- (colSums(Tau.hat)-1)/(d-1)
tau.bar <- mean(tau.col)
tau.star <- (d-1)/(d-2)*(tau.col[ij.mat[,1]] + tau.col[ij.mat[,2]]) - d/(d-2)*tau.bar

# components of Hajek projection
HP <- computeHajekProjection(X)

Tcs <- sapply(HP, function(Th) (colSums(Th)-1)/(d-1))
Ths <- sapply(HP, function(Th) Th[ij.mat])
Tbs <- colMeans(Ths)

## CHECKUP -- FINE
#averageSigma(computeSigmaJackknife(HP,ij.mat)$ShJ,l.mat)
#computeSigma3Jackknife(Ths,Tcs,Tbs)
sigma.hat <- computeSigma3Jackknife(Ths,Tcs,Tbs)
delta.hat <- c(rbind(c(p-2*d+3,2*(d-2),1),c(3-d,d-4,1),c(1,-2,1)) %*% sigma.hat)



### NEW TEST
M <- 10000
Z <- matrix(rnorm(M*p),p,M)
Zc <- apply(Z,2,function(z){
  sapply(1:d, function(k) mean(z[l.mat[k,-k]]))
})
Zb <- apply(Z,2,mean)

ZZ <- sqrt(delta.hat[3])*t(Z) +
  (sqrt(delta.hat[2]) - sqrt(delta.hat[3]))*((d-1)/(d-2))*t(Z[ij.mat[,1],]+Z[ij.mat[,2],]) +
  (d*sqrt(delta.hat[3]) - 2*(d-1)*sqrt(delta.hat[2]))/(d-2)*Zb

ss <- averageSigma(cov(ZZ),l.mat)
SbJ <- computeSigmaJackknife(HP,ij.mat,T,l.mat)$SbJ
sss <- averageSigma((diag(p) - 1/p) %*% SbJ,l.mat)

rbind(ss,
      sss)

# rbind(ss,
#       sigma.hat)
# rbind(c(rbind(c(p-2*d+3,2*(d-2),1),c(3-d,d-4,1),c(1,-2,1)) %*% ss),
#       delta.hat)





# Z1 <- replicate(M, {rnorm(d,0,sqrt(sigma.hat[2]-sigma.hat[1]))})
Z1 <- replicate(M, {rnorm(d,0,sqrt(max(sigma.hat[2]-sigma.hat[1],0)))})
Z2 <- replicate(M, {rnorm(p,0,sqrt(delta.hat[3]))})
Z.hat <- Z2 + Z1[ij.mat[,1],] + Z1[ij.mat[,2],]
Z.hat <- (diag(p) + 1/p) %*% Z.hat
ss <- averageSigma(cov(t(Z.hat)),l.mat)
rbind(ss,sigma.hat)








#### TEST2

SbJ <- computeSigmaJackknife(HP,ij.mat,T,l.mat)$SbJ
ZZ <- rmvnorm(20000,sigma=SbJ)

averageSigma(cov(ZZ),l.mat)
sigma.hat

averageSigma((diag(p) - 1/p) %*% cov(ZZ),l.mat)
averageSigma((diag(p) - 1/p) %*% SbJ,l.mat)





B <- Matrix::Matrix(0, nrow = p, ncol = d, sparse = T)
for(i in 1:d){
  B[l.mat[i,-i],i] <- 1
}
B <- as.matrix(B)
Gs <- B %*% MASS::ginv(B)
G <- matrix(1/p,p,p)

IGs <- diag(p) - Gs
GsG <- Gs - G

ZZ <- sqrt(delta.hat[3])*IGs %*% Z + sqrt(delta.hat[2])*GsG %*% Z

averageSigma(cov(t(ZZ)),l.mat)
averageSigma((diag(p) - 1/p) %*% SbJ,l.mat)


c(Gs %*% Z[,1]) -
((d-1)/(d-2)*(Zc[ij.mat[,1],1] + Zc[ij.mat[,2],1]) - d/(d-2)*Zb[1])

ZZ <- sqrt(delta.hat[3])*
  (t(Z) - (d-1)/(d-2)*t(Zc[ij.mat[,1],] + Zc[ij.mat[,2],]) + d/(d-2)*Zb) +
  sqrt(delta.hat[2])*
  ((d-1)/(d-2)*t(Zc[ij.mat[,1],] + Zc[ij.mat[,2],]) - 2*(d-1)/(d-2)*Zb)


averageSigma(cov(ZZ),l.mat)
averageSigma((diag(p) - 1/p) %*% SbJ,l.mat)


ZZ <- sqrt(delta.hat[3])*t(Z) +
  (sqrt(delta.hat[2]) - sqrt(delta.hat[3]))*((d-1)/(d-2))*t(Zc[ij.mat[,1],] + Zc[ij.mat[,2],]) +
  (sqrt(delta.hat[3])*d/(d-2) - sqrt(delta.hat[2])*2*(d-1)/(d-2))*Zb

averageSigma(cov(ZZ),l.mat)
averageSigma((diag(p) - 1/p) %*% SbJ,l.mat)


Zp <- sqrt(delta.hat[3])*
  (t(Z) - (d-1)/(d-2)*t(Zc[ij.mat[,1],] + Zc[ij.mat[,2],]) + d/(d-2)*Zb)
Zd <- sqrt(delta.hat[2])*
  ((d-1)/(d-2)*t(Zc[ij.mat[,1],] + Zc[ij.mat[,2],]) - 2*(d-1)/(d-2)*Zb)
Z <- Zp + Zd

averageSigma(cov(Z),l.mat)
averageSigma((diag(p) - 1/p) %*% SbJ,l.mat)

averageSigma(cov(Zp),l.mat)
averageSigma(IGs %*% SbJ,l.mat)

averageSigma(cov(Zd),l.mat)
averageSigma(GsG %*% SbJ,l.mat)
