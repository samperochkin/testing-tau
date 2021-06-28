d <- 4
ij.mat <- t(combn(d,2))
theta <- .4


R <- diag(d)*(1-theta) + theta
x <- rnorm(d)
y <- rnorm(d)
ij <- c(2,4)

# normal

Ri <- solve(R)
XRi <- rbind(x) %*% Ri
XRiERiX <- apply(ij.mat,1,function(ij){
  E <- matrix(0,d,d)
  E[rbind(ij,rev(ij))] <- 1
  sum((XRi %*% E) * XRi)
})
sx <- - Ri[ij.mat] + XRiERiX/2

XRi <- rbind(y) %*% Ri
XRiERiX <- apply(ij.mat,1,function(ij){
  E <- matrix(0,d,d)
  E[rbind(ij,rev(ij))] <- 1
  sum((XRi %*% E) * XRi)
})
sy <- - Ri[ij.mat] + XRiERiX/2


Ri <- solve(R)
XRi <- rbind(x,y) %*% Ri
XRiERiX <- apply(ij.mat,1,function(ij){
  E <- matrix(0,d,d)
  E[rbind(ij,rev(ij))] <- 1
  sum((XRi %*% E) * XRi)
})
- 2*Ri[ij.mat] + XRiERiX/2
sx+sy


# cauchy
Ri <- solve(R)

XRi <- rbind(x) %*% Ri
XRiERiX <- apply(ij.mat,1,function(ij){
  E <- matrix(0,d,d)
  E[rbind(ij,rev(ij))] <- 1
  sum((XRi %*% E) * XRi)
})
XRiX <- sum(XRi * rbind(x))
sx <- - Ri[ij.mat] + ((d+4)/2) * XRiERiX/(4 + XRiX)

XRi <- rbind(y) %*% Ri
XRiERiX <- apply(ij.mat,1,function(ij){
  E <- matrix(0,d,d)
  E[rbind(ij,rev(ij))] <- 1
  sum((XRi %*% E) * XRi)
})
XRiX <- sum(XRi * rbind(y))
sy <- - Ri[ij.mat] + ((d+4)/2) * XRiERiX/(4 + XRiX)


XRi <- rbind(x,y) %*% Ri
XRiX <- rowSums(XRi * rbind(x,y))
XRiERiX <- apply(ij.mat,1,function(ij){
  E <- matrix(0,d,d)
  E[rbind(ij,rev(ij))] <- 1
  sum(((XRi %*% E) * XRi)/(4 + XRiX))
})
- 2*Ri[ij.mat] + ((d+4)/2) * XRiERiX
sx+sy



# HAC
