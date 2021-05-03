library(mvtnorm)

n <- 500
d <- 3
p <- choose(d,2)
s <- .25
eps0 <- .3
epsn <- 1 + eps0/sqrt(n)

X <- rmvnorm(n, rep(0,d), (1-s)*diag(d) + s)
tau.hat <- pcaPP::cor.fk(X)[cbind(c(1,1,2),c(2,3,3))]


B <- as.matrix(rep(1,p))
C <- B
C[1] <- C[1] + eps0/sqrt(n)

G <- tcrossprod(B,B)/c(crossprod(B,B))
H <- tcrossprod(C,C)/c(crossprod(C,C))

G %*% tau.hat
H %*% tau.hat

# GOOD!
H[1:2,1:2]
1/(epsn^2 + p - 1)*epsn^2 # (1,1)
1/(epsn^2 + p - 1)*epsn # (1,2)
1/(epsn^2 + p - 1) # (2,2)

# GOOD!
(H - G)[1:2,1:2]
1/(p*(epsn^2 + p - 1))*(p-1)*(epsn^2-1) # (1,1)
1/(p*(epsn^2 + p - 1))*(epsn*p - epsn^2 - p + 1) # (1,2)
1/(p*(epsn^2 + p - 1))*(1 - epsn^2) # (2,2)

# or.. GOOD!
(H - G)[1:2,1:2]
(epsn - 1)/(p*(epsn^2 + p - 1))*(p-1)*(epsn+1) # (1,1)
(epsn - 1)/(p*(epsn^2 + p - 1))*(p - epsn - 1) # (1,2)
(epsn - 1)/(p*(epsn^2 + p - 1))*(-1 - epsn) # (2,2)

# or.. GOOD!
sqrt(n)*(H - G)[1:2,1:2]
1/p^2*(2*eps0*(p-1)) # (1,1)
(p-2)*eps0/p^2 # (1,2)
-2*eps0/p^2 # (2,2)




sqrt(n)*(H - G) %*% rep(1,p)
eps0*(p-1)/p
-eps0/p
