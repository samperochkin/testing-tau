Tau <- S

library(mvtnorm)
library(pcaPP)

n <- 150
d <- ncol(Tau)
X <- rmvnorm(n, sigma = sin(pi*Tau/2))

Tau.hat <- cor.fk(X)
image(t(Tau.hat[d:1,]), col=rainbow(100))







