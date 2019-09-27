library(HAC)
library(mvtnorm)
library(pcaPP)
library(Matrix)

source("functions/generateData.R")
# source("functions/buildSigma.R")
# source("functions/computeTh.R")
# source("functions/computeTh4.R")
# source("functions/computeTh5.R")


X <- generateData(n = 200,
                  d = 300,
                  tau = .3,
                  dtau = 0,
                  distribution = "joe")

n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2
l.ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p

Tau.hat <- cor.fk(X)
tau.hat <- Tau.hat[l.ij.mat]
tau.bar <- mean(tau.hat)

B <- Matrix(0, nrow = p, ncol = d, sparse = T)
for(i in 1:d){
  B[ij.l.mat[i,-i],i] <- 1
}
tb <- as.vector(B %*% (solve(crossprod(B)) %*% crossprod(B,tau.hat)))


tbb <- sapply(1:d, function(i){
  mean(Tau.hat[i,-i])
})

v <- (tb - outer(tbb,tbb,"+")[l.ij.mat] + tau.bar)
hist(v, breaks=25, probability=T)

ss <- seq(-5,5,.001)
lines(ss, dnorm(ss,sd=sd(v)))

tau.hat - outer(tbb,tbb,"+")[l.ij.mat] + tau.bar
tau.hat - tb


v <- (tb - tau.hat)
hist(v, breaks=25, probability=T)

ss <- seq(-5,5,.001)
lines(ss, dnorm(ss,sd=sd(v)))



# G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B))
# 
# G[1,1]
# G[1,2]
# G[1,p]
# 
# 2/(d-1)
# (d-3)/((d-1)*(d-2))
# -2/((d-1)*(d-2))



hist(tau.hat- tb)
hist(tau.hat- tau.bar)
hist(tb- tau.bar)


library(ggplot2)
vv <- c(tau.hat-tau.bar,
        tau.hat - outer(tbb,tbb,"+")[l.ij.mat] + tau.bar,
        tau.hat-tb,
        tb-tau.bar)


ggplot(data.frame(x=vv, y=rep(c("a","b","c","d"), each=p)), aes(x = x, fill = y)) +
  geom_density(alpha = .25)

ggplot(data.frame(x=vv, y=rep(c("a","b","c","d"), each=p))[(p+1):(3*p),], aes(x = x, fill = y)) +
  geom_density(alpha = .25)

plot(matrix(vv[(p+1):(3*p)],p,2))
