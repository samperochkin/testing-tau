n <- 1000
p <- 10
epsilon <- 1
epsilon_n <- epsilon/sqrt(n)

B <- rep(1,p)
B
BB <- B %*% MASS::ginv(B)
BB

C <- c(1 + epsilon_n, rep(1,p-1))
C
CC <- C %*% MASS::ginv(C)
CC

tcrossprod(B)/c(crossprod(B)) - BB
tcrossprod(C)/c(crossprod(C)) - CC

CC - BB

denum <- p*((1+epsilon_n)^2  + p - 1)

num11 <- (p-1)*(1+epsilon_n)^2 + 1 - p
num12 <- 1 + p*epsilon_n - (1+epsilon_n)^2
num22 <- 1 - (1+epsilon_n)^2

num11/denum
num12/denum
num22/denum
