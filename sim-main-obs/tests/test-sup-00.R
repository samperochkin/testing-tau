# X <- mvtnorm::rmvnorm(200,rep(0,5),diag(5)*(.3) + .7)
X <- mvtnorm::rmvnorm(300,rep(0,10),diag(10))

n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p


# computation of estimates ------------------------------------------------
Tau.hat <- cor.fk(X)
t.hat <- Tau.hat[ij.mat]
t.bar <- mean(t.h)at

Th <- computeTh(X)
# Sh <- n*buildSigma(Th,rep(t.bar,p),n,F)
Sh <- n*buildSigma(Th,t.hat,n,F)
Shi <- solve(Sh)

eig <- eigen(Sh)
eig$values

Shh <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
Shih <- solve(Shh)

B <- rep(1,p)


library(MASS)
S1 <- Shh %*% t(ginv(B)) %*% t(B) %*% Shih
S2 <- Shih %*% B %*% ginv(B) %*% Shh

S1 - S2


Shih %*% B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shih - (Shih %*% B) %*% ginv(Shih %*% B)
Shih %*% B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shih - Shih %*% B %*% ginv(B) %*% Shh


(Shih %*% B) %*% ginv(Shih %*% B) - 0

ginv(Shih %*% B) - ginv(B) %*% Shh


(Shih %*% B) %*% ginv(Shih %*% B)
(Shih %*% B) %*% ginv(B) %*% Shh
(Shih %*% B) %*% ginv(Shih %*% B)-(Shih %*% B) %*% ginv(B) %*% Shh



Shih %*% B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shih



Shih %*% B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shih
(Shih %*% B) %*% ginv(Shih %*% B)

(Shih %*% B) %*% ginv(Shih %*% B)


((Shih %*% B) %*% ginv(Shih %*% B)) %*% ((Shih %*% B) %*% ginv(Shih %*% B)) - ((Shih %*% B) %*% ginv(Shih %*% B))



B %*% ginv(B)

ginv(Shih %*% B)
ginv(t(B) %*% Shh)
#


G <- Shih %*% B
Gg <- ginv(G)

(G %*% Gg %*% G) - G
(Gg %*% G %*% Gg) - Gg
Gg %*% G - t(Gg %*% G)
G %*% Gg - t(G %*% Gg)
