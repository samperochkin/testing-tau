library("mvtnorm")
library(pcaPP)


# Levels are the same, but power is better with Monte Carlo

source("sim-main/functionsLow/generateData.R")
X <- generateData(n=65, d=18, tau=.3, dtau=.4, dtau_type = "column", distribution = "normal")
M <- 5000

# setup -------------------------------------------------------------------
n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p


# computation of estimates ------------------------------------------------
Tau.hat <- cor.fk(X)
t.hat <- Tau.hat[ij.mat]
t.col <- (colSums(Tau.hat)-1)/(d-1)
t.bar <- mean(t.col)
t.star <- (d-1)/(d-2)*(t.col[ij.mat[,1]] + t.col[ij.mat[,2]]) - d/(d-2)*t.bar
t.cen <- t.hat-t.bar

# components of Hajek projection
T.hajek <- lapply(1:n, function(r){
  V <- t(X[r,] < t(X[-r,]))
  V <- Reduce("+",lapply(1:nrow(V), function(k){
    4*(outer(V[k,],V[k,],"=="))
  }))
  V
})


# Tcs <- sapply(T.hajek, function(Th) (colSums(Th)-1)/(d-1))
Ths <- sapply(T.hajek, function(Th) Th[ij.mat])
Tbs <- apply(Ths, 2, mean)
Tcens <- Ths-matrix(Tbs,p,n,byrow=T)

# sigma <- c(
#   mean(apply(Ths, 1, var))/(n*(n-1)),
#   mean(apply(Tcs, 1, var))/(n*(n-1)),
#   var(Tbs)/(n*(n-1))  
# )
# sigma <- c(sigma %*% matrix(c(1,0,0,
#                               -1/(d-2),(d-1)/(d-2),0,
#                               1/(p-2*d+3),-2*(d-1)/(p-2*d+3),p/(p-2*d+3)),3,3))
# delta <- rbind(c(1,-2,1),c(1,d-4,3-d)) %*% sigma



#### MC
# Z1 <- replicate(M, {rnorm(d,0,sqrt(sigma[2]-sigma[3]))})
# Z2 <- replicate(M, {rnorm(p,0,sqrt(delta[1]))})
# Z.hat <- Z2 + Z1[ij.mat[,1],] + Z1[ij.mat[,2],]
Z.hat <- t(rmvnorm(n=M,rep(0,p),sigma=cov(t(Ths))/(n*(n-1))))
Z.bar <- colMeans(Z.hat)
# Z.col <- t(sapply(1:d, function(i) colMeans(Z.hat[ij.l.mat[,i],])))
# Z.star <- t(t((d-1)/(d-2)*(Z.col[ij.mat[,1],] + Z.col[ij.mat[,2],])) - d/(d-2)*Z.bar)

# MC.euc.I <- delta[1]*rchisq(M, p-d) + delta[2]*rchisq(M, d-1)


S <- (diag(p) - 1/p) %*% (cov(t(Ths))/(n*(n-1))) %*% (diag(p) - 1/p)
ev <- eigen(S)$val
MC.euc.I <- c(ev %*% matrix(rchisq(M*p, 1),p,M))

MC.sup.I <- apply(t(Z.hat) - Z.bar, 1, function(z) max(abs(z)))
MC.sup.S <- replicate(M, {z <- rnorm(p); max(abs(z-mean(z)))})


#### Boot
Boot <- replicate(M, {
  zz <- sqrt(n)*(Tcens/(n*(n-1))  - t.cen/n) %*% rnorm(n)
  
  Boot.euc.I <- crossprod(zz)
  Boot.sup.I <- max(abs(zz))
  c(Boot.euc.I=Boot.euc.I, Boot.sup.I=Boot.sup.I)
})

library(data.table)
df <- data.table(method = rep(c("MC","Boot"), each=2*M),
                 test_stat = c(rep(c("euc","sup"), each=M),rep(c("euc","sup"), each=M)),
                 score = c(MC.euc.I,MC.sup.I,Boot[1,],Boot[2,]))


library(ggplot2)
ggplot(df, aes(x=score, fill=method)) +
  geom_density(alpha = .5) +
  facet_wrap(~test_stat, scales="free")


ssup <- sqrt(n)*max(t.cen)
eeuc <- n*c(crossprod(t.cen))

mean(eeuc < df[method == "MC" & test_stat == "euc", score])
mean(eeuc < df[method == "Boot" & test_stat == "euc", score])

mean(ssup < df[method == "MC" & test_stat == "sup", score])
mean(ssup < df[method == "Boot" & test_stat == "sup", score])

