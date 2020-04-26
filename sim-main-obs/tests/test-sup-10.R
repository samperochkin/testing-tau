n <- 100
d <- 6

X1 <- rnorm(n,0,1)
X2 <- rmvnorm(n,rep(0,2),diag(2)*.4+.6) + X1/2
X3 <- rmvnorm(n,rep(0,3),diag(3)*.6+.4) + rowMeans(X2)/2

X <- cbind(X1,X2,X3)
# X <- mvtnorm::rmvnorm(300,rep(0,10),diag(10))

p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[rbind(ij.mat,ij.mat[,2:1])] <- 1:p


# computation of estimates ------------------------------------------------
Tau.hat <- cor.fk(X)
image(t(Tau.hat[d:1,]), zlim = c(0,1))

D <- matrix(0,d,3)
D[1,1] <- 1
D[2:3,2] <- 1
D[4:6,3] <- 1

kk.mat <- rbind(cbind(1:3,1:3),t(combn(3,2)))
B <- matrix(0,p,nrow(kk.mat))

for(i in 1:nrow(kk.mat)){
  kk <- kk.mat[i,]
  B[tcrossprod(D[,kk[1]],D[,kk[2]])[ij.mat] == 1,i] <- 1
}
if(any(colSums(B)==0)) B <- B[,-which(colSums(B)==0)]


BBp <- B %*% ginv(B)

t.hat <- Tau.hat[ij.mat]
t.bar <- BBp %*% t.hat

Th <- computeTh(X)
# Sh <- n*buildSigma(Th,rep(t.bar,p),n,F)
Sh <- n*buildSigma(Th,t.hat,n,F)
Shi <- solve(Sh)

eig <- eigen(Sh)
eig$values

Shh <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
Shih <- solve(Shh)

# B <- rep(1,p)

