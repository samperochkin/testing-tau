# verify derivative of log.c -- normal case
d <- 4

R <- diag(d)*(1-theta) + theta
Ri <- (diag(d)- theta/(1 + (d-1)*theta))/(1-theta)

log.c <- function(delta){
  R[rbind(ij,rev(ij))] <- R[rbind(ij)] + delta
  log(c(1/sqrt(det(R)) * exp(-1/2 * t(x) %*% (solve(R) - diag(d)) %*% x)))
}

x <- rnorm(d)
ij <- c(2,4)
ss <- sapply(seq(-.001,.001,.0001), log.c)
mean(diff(ss)[10:11])/.0001


Ri <- (diag(d)- theta/(1 + (d-1)*theta))/(1-theta)
XRi <- rbind(x) %*% Ri
XRiERiX <- apply(ij.mat,1,function(ij){
  E <- matrix(0,d,d)
  E[rbind(ij,rev(ij))] <- 1
  sum((XRi %*% E) * XRi)
})
- Ri[ij.mat] + XRiERiX/2


# verify derivative of log.c -- Cauchy case (make sure proportional density is okay*)
d <- 4

R <- diag(d)*(1-theta) + theta
Ri <- (diag(d)- theta/(1 + (d-1)*theta))/(1-theta)

log.c <- function(delta){
  R[rbind(ij,rev(ij))] <- R[rbind(ij)] + delta
  - (1/2) * log(det(R)) - ((d+1)/2) * log(1 + t(x) %*% solve(R) %*% x)
}

x <- rnorm(d)
ij <- c(1,4)
ss <- sapply(seq(-.001,.001,.0001), log.c)
mean(diff(ss)[10:11])/.0001


Ri <- (diag(d)- theta/(1 + (d-1)*theta))/(1-theta)
XRi <- rbind(x) %*% Ri
XRiERiX <- apply(ij.mat,1,function(ij){
  E <- matrix(0,d,d)
  E[rbind(ij,rev(ij))] <- 1
  sum((XRi %*% E) * XRi)
})
XRiX <- rowSums(XRi * rbind(x))
- Ri[ij.mat] + ((d+1)/2) * XRiERiX/(1 + XRiX)
