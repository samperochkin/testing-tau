library("mvtnorm")
library(pcaPP)


# Levels are the same, but power is better with Monte Carlo

source("sim-main/functionsLow/generateData.R")
X <- generateData(n=150, d=5, tau=0, dtau=.2, dtau_type = "column", distribution = "normal")
M <- 50000

n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
l.mat <- matrix(0,d,d)
l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p

Tau.hat <- pcaPP::cor.fk(X)
tau.hat <- Tau.hat[ij.mat]

sapply(list.files("sim-main/functionsLow", full.names = T), source, local=environment())

HP <- computeHajekProjection(X,ij.mat)
Sh <- computeSigmaJackknife(HP, ij.mat, F)$ShJ

B <- rep(1,p)
IBB <- diag(p) - matrix(1/p,p,p)

isPD <- matrixcalc::is.positive.definite(Sh)
isPSD <- matrixcalc::is.positive.semi.definite(Sh)
  
SI.star <- IBB %*% Sh %*% IBB
SI.star <- (SI.star + t(SI.star))/2

MC <- cbind(
  # matrix(rchisq(M*p,1),M,p) %*% eigen(SI.star,T,T)$values,
  apply(rmvnorm(M,rep(0,p),SI.star),1,crossprod),
  apply(abs(rmvnorm(M,rep(0,p),SI.star)),1,max)
)


Tbs <- IBB %*% HP
tb <- c(IBB %*% tau.hat)
  
TT <- Tbs/sqrt(n-1) - tb/sqrt(n)
boot <- replicate(M, {
  zz <- (TT %*% rnorm(n))
  
  c(euc=crossprod(zz),
    supI=max(abs(zz)))
})


library(data.table)
df <- data.table(method = rep(c("MC","boot"), each=2*M),
                 test_stat = c(rep(c("euc","sup"), each=M),rep(c("euc","sup"), each=M)),
                 score = c(MC,t(boot)))


library(ggplot2)
ggplot(df, aes(x=score, fill=method)) +
  geom_density(alpha = .5) +
  facet_wrap(~test_stat, scales="free")


ssup <- sqrt(n)*max(tb)
eeuc <- n*c(crossprod(tb))


mean(eeuc < df[method == "MC" & test_stat == "euc", score])
mean(eeuc < df[method == "boot" & test_stat == "euc", score])

mean(ssup < df[method == "MC" & test_stat == "sup", score])
mean(ssup < df[method == "boot" & test_stat == "sup", score])

