testEquiRankMC <- function(X, M){
  
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
  
  s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  s10 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/(d*(d-1))
  
  # s210 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/(p-d)
  # s10 <- n*crossprod(t.col - t.bar)/(d-1)

  
  # MCI <- matrix(rnorm(M*p),p,M)
  MCS <- replicate(M, {
    Z1 <- rnorm(d,0,sqrt(s10))
    Z2 <- rnorm(p,0,sqrt(s210))
    Z2 + Z1[ij.mat[,1]] + Z1[ij.mat[,2]]
  })
  
  supFun <- function(tt){max(abs(tt-mean(tt)))}
  # eucFun <- function(tt){c(crossprod(tt-mean(tt)))}
  
  supMCS <- apply(MCS, 2, supFun)
  psup <-  mean(supMCS > sqrt(n)*max(abs(t.hat-t.bar)))
  
  # eucMCS <- apply(MCS, 2, eucFun)
  # peuc <- mean(eucMCS > n*c(crossprod(t.hat-t.bar)))
  # 
  # mahaMCI <- apply(MCI, 2, eucFun)
  # 
  # e2 <- c(s210 + (d-4)*s10)
  # 
  # l1 <- n*crossprod(t.hat - t.col[ij.mat[,1]] - t.col[ij.mat[,2]] + t.bar)/s210
  # l2 <- n*crossprod(t.col[ij.mat[,1]] + t.col[ij.mat[,2]] - 2*t.bar)/e2
  # pmaha <- mean(mahaMCI > (p-d) + e2)
  
  # c(psup,peuc,pmaha)
  psup
}
