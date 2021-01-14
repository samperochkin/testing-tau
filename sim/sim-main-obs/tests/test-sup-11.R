# B <- rep(1,p)
I <- diag(p)
BBp <- B %*% ginv(B)
# G <- B %*% ginv(B)
# IG <- I - G
# SS <- IG
G <- B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shi
IG <- diag(p) - G
SS <- diag(p) - Shih %*% G %*% Shh
SS <- (t(SS) + SS)/2

L <- ncol(B)

M <- 10000
Z1 <- rmvnorm(M,rep(0,p),SS)
# Z2 <- rmvnorm(M,rep(0,p-L),diag(p-L))
# Ss <- round(Ss)
# Z2 <- rmvnorm(M,rep(0,p-L),diag(p-L))
Z2 <- rmvnorm(M,rep(0,p),diag(p) - BBp)

# m1 <- apply(abs(Z1 - rowMeans(Z1)), 1, max)
m1 <- apply(abs(Z1), 1, max)
# m2 <- apply(abs((diag(p) - G)), 1, max)
m2 <- apply(abs(Z2), 1, max)

# plot(sort(m1),sort(m2), type = "l")
# lines(c(0,10), c(0,10), col="red")
# lines(range(m1),range(m2), col="blue")
par(mar = c(2,2,1,1))
plot(sort(m1),sort(m2), type = "l", col="blue")
points(sort(m1),sort(m2), pch = 19, cex=.1)
lines(c(0,10), c(0,10), col="red")
# lines(range(m1),range(m2), col="blue")
abline(v = quantile(m1,prob=c(.01,.99)), lty=2)
abline(h = quantile(m2,prob=c(.01,.99)), lty=2)

apply(rbind(m1,m2),1,quantile, prob = c(.05,.25,.5,.75,.9,.95,.975,.99))




W <- eigen(SS)$vec
# Ss <- t(W[,1:(p-L)]) %*% SS %*% W[,1:(p-L)]
Ss <- t(W) %*% SS %*% W
M <- 10000
Z1 <- rmvnorm(M,rep(0,p),SS)
Z2 <- rmvnorm(M,rep(0,p-1),Ss)

m1 <- apply(abs(Z1), 1, max)
m2 <- apply(abs(Z2 - rowMeans(Z2)), 1, max)

# plot(sort(m1),sort(m2), type = "l")
# lines(c(0,10), c(0,10), col="red")
# lines(range(m1),range(m2), col="blue")
plot(sort(m1),sort(m2), type = "l")
lines(c(0,10), c(0,10), col="red")
lines(range(m1),range(m2), col="blue")


apply(rbind(m1,m2),1,quantile, prob = c(.05,.25,.5,.75,.9,.95,.975,.99))


