n <- 150
d <- 30
S <- diag(d)*.6 + .4

X <- rmvnorm(n,rep(0,d),S)
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
t.cen <- t.hat - t.bar

C1 <- lapply(1:n, function(r){
  v <- t(X[r,] < t(X[-r,]))
  v <- rowSums(apply(v, 1, function(v){
    4*(outer(v,v,"=="))[ij.mat]
  }))
  V <- diag(d)
  V[ij.mat] <- V[ij.mat[,2:1]] <- v
  V
})

Tcs1 <- sapply(C1, function(Th) (colSums(Th)-1)/(d-1))
Ths1 <- sapply(C1, function(Th) Th[ij.mat])
Tbs1 <- apply(Ths1, 2, mean)

s12 <- mean(apply(Ths1, 1, var))/(n*(n-1))
s11 <- mean(apply(Tcs1, 1, var))/(n*(n-1))
s10 <- var(Tbs1)/(n*(n-1))
sc1 <- c(s12,s11,s10)

sj <- jackknifeVariance(X)

Theta <- computeTh(X)
Sh <- buildSigma(Theta, rep(mean(cor.fk(X)[ij.mat]),p), n, F)
Shh <- buildSigma(Theta, cor.fk(X)[ij.mat], n, F)
B <- Matrix(0, nrow = p, ncol = d, sparse = T)
for(i in 1:d){
  B[ij.l.mat[i,-i],i] <- 1
}
BtB <- B %*% t(B)
G <- as.matrix(B %*% solve(crossprod(B)) %*% t(B))

sp1 <- numeric(3)
sp2 <- numeric(3)
# Averaging of Sh
for(i in 1:3){
  ind <- which(BtB == i-1)
  # den <- (i==1)*p*(p-2*d+3) + (i==2)*2*d*(p-d) + (i==3)*(p-1)
  # sp1[4-i] <- n*sum(Sh[ind])/den
  # sp2[4-i] <- n*sum(Shh[ind])/den
  sp1[4-i] <- n*mean(Sh[ind])
  sp2[4-i] <- n*mean(Shh[ind])
}



sc1
sj
sp1
sp2


c(
c(1,-2,1) %*% sc1,
c(1,-2,1) %*% sp1,
c(1,-2,1) %*% sp2
)

c(
c(0,1,-1) %*% sc1,
c(0,1,-1) %*% sp1,
c(0,1,-1) %*% sp2
)
# sc1 - (n-1)^(-2)*sj*((n-2)^2/(n*(n-1)))
# sc1 - sj*((n-2)^2/(n*(n-1)))

sum((Sh - cov(t(Ths1))/(n*(n-1)^2))^2)
sum((Shh - cov(t(Ths1))/(n*(n-1)^2))^2)

max(abs(Sh - cov(t(Ths1))/(n*(n-1)^2)))
max(abs(Shh - cov(t(Ths1))/(n*(n-1)^2)))


sc1 - sp1*(n)^2/(n-2)^2
sc1 - sp2*(n)^2/(n-2)^2

