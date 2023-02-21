X <- matrix(rnorm(100),20,5)

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
source("sim/sim-main/functionsHigh2/computeHajekProjection.R")
HP <- computeHajekProjection(X)

Tcs <- sapply(HP, function(Th) (colSums(Th)-1)/(d-1))
Ths <- sapply(HP, function(Th) Th[ij.mat])
Tbs <- colMeans(Ths)

source("sim/sim-main/functionsHigh2/computeSigma3Jackknife.R")
sigma.hat <- computeSigma3Jackknife(Ths,Tcs,Tbs)
delta.hat <- c(rbind(c(p-2*d+3,2*(d-2),1),c(3-d,d-4,1),c(1,-2,1)) %*% sigma.hat)


#### losses

euc <- c(n*crossprod(tau.hat-tau.bar))
maha_p <- c(n*crossprod(tau.hat-tau.star))/delta.hat[3]
maha_d <- (d-1)^2/(d-2)*c(n*crossprod(tau.col-tau.bar))/delta.hat[2]
maha <- maha_d + maha_p 

(d-1)^2/(d-2)*crossprod(tau.col-tau.bar)
crossprod(tau.star-tau.bar)

(d-1)/(d-2)*sum(tau.col[1:2]-tau.bar)
(tau.star-tau.bar)[1]

G <- matrix(1/p,p,p)
G.star <- matrix(0,p,p)
R <- apply(ij.mat, 1, function(ij){
  apply(ij.mat, 1, function(kl) length(intersect(ij,kl)))
})
for(r in 0:2) G.star[R==r] <- c(-2/(d-2), (d-3)/(d-2), 2)[r+1]/(d-1)

I <- diag(p)
IG <- I - G.star
GG <- G - G.star

IG %*% GG
t(IG %*% tau.hat) %*% (GG %*% tau.hat)

source("sim/sim-main/functionsLow2/averageSigma.R")
source("sim/sim-main/functionsLow2/computeSigmaJackknife.R")
Sigma.hat <- c(computeSigmaJackknife(HP, ij.mat, T, l.mat))
Sb <- Sigma.hat$SbJ
eig <- eigen(Sb, symmetric = T)


Sh2 <- eig$vectors[,keep] %*% diag(sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
Shi <- eig$vectors[,keep] %*% diag(1/eig$values[keep]) %*% t(eig$vectors[,keep])
Shi2 <- eig$vectors[,keep] %*% diag(1/sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])

SI.star2 <- IBB %*% Sh2 
IG <- diag(p)- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)

