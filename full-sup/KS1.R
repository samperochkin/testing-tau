n <- 150
d <- 50

X <- rnorm(n,0,1) + matrix(rnorm(n*d,0,1.5),n,d)
#X[,1:15] <- X[,1:15] + rnorm(n,0,3)

# setup -------------------------------------------------------------------
n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
# ij.l.mat <- matrix(0,d,d)
# ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p


# computation of estimates ------------------------------------------------
Tau.hat <- cor.fk(X)
t.hat <- Tau.hat[ij.mat]
t.col <- (colSums(Tau.hat)-1)/(d-1)
t.bar <- mean(t.col)

# s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
# s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))
s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))
#############

MCS1 <- replicate(M, {
  Z1 <- rnorm(d,0,sqrt(s10))
  Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
})

MCS2 <- replicate(M, {
  rnorm(p,0,sqrt(s210))
})

inds <- sapply(1:d, function(i) which(apply(ij.mat,1, function(ij) i %in% ij)))
circ <- function(tt){
  t.circ <- apply(inds, 2, function(ind) mean(tt[ind]))
}  

MCS.circ <- apply(MCS, 2, circ)
th.circ <- circ(t.hat)

plot(th.circ-mean(th.circ))
pvals1 <- pnorm(th.circ,
                mean(th.circ),
                sd(th.circ))
hist(pvals1)


plot(t.hat - th.circ[ij.mat[,1]] - th.circ[ij.mat[,2]] + mean(th.circ))
pvals2 <- pnorm(t.hat,
                th.circ[ij.mat[,1]] + th.circ[ij.mat[,2]] - mean(th.circ),
                sd(t.hat-th.circ[ij.mat[,1]] + th.circ[ij.mat[,2]] - mean(th.circ))/sqrt(2))
hist(pvals2)

pvals <- c(pvals1,pvals2)
plot(pvals)
plot(sort(pvals), type="l")

# 
# pvals1 <- pmin(pvals1,1-pvals1)
# pvals2 <- pmin(pvals2,1-pvals2)
# 
# min(p.adjust(c(pvals1,pvals2)))
ks.test(pvals,"punif")
(p+d)-length(unique(pvals))
