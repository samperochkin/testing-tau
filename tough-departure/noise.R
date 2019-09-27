library(mvtnorm)
n <- 100
d <- 100
# v <- rnorm(d,.3,.04)
# V <- rnorm(d,.3,.04)
# S <- V - diag(diag(V)) + diag(d)

V <- matrix(rnorm(d^2,.6,sqrt(.1)),d,d)
V <- (V + t(V))/2
S <- V - diag(diag(V)) + diag(d)

S <- as.matrix(nearPD(S, corr = T)$mat)


X <- rmvnorm(n, sigma=S)

par(mfrow=c(1,3))
hist(cor(X))
hist(cor(rmvnorm(n, sigma=.4*diag(d)+.6)))
hist(S)

testEquiRankMC(X,500)
# testEquiRank(X)



