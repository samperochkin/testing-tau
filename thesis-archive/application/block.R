X <- X[,nn]
X <- X[,1:30]

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

# Boot
C.gen.raw <- sapply(1:n, function(r){
  V <- t(X[r,] < t(X[-r,]))
  rowSums(apply(V, 1, function(v){
    4*(outer(v,v,"=="))[ij.mat]
  }))/(n*(n-1))
})


# B
B <- diag(d)

constructBs <- function(B,kk.mat){
  lapply(1:nrow(kk.mat), function(kk){
    kk <- kk.mat[kk,]
    B[,kk[1]] <- rowSums(B[,kk])
    B[,-kk[2]]
  })
}

computeP <- function(B){
  K <- ncol(B)
  clus <- c(B %*% seq(K))
  
  block <- c((matrix(clus[ij.mat],ncol=2)-1) %*% c(K,1)) + 1
  G <- matrix(0,p,max(block))
  G[cbind(1:p,block)] <- 1
  ss <- colSums(G)
  if(any(ss == 0)) G <- G[,-which(ss==0)]

  t.cen <- (t.hat - G %*% (crossprod(G,t.hat)/colSums(G)))
  
  C.gen <- apply(C.gen.raw,2,function(v){
    (v - G %*% (crossprod(G,v)/colSums(G)) )
  }) - c(t.cen/n)

  boot.gen <- replicate(1000, {
    rowSums(C.gen %*% rnorm(n))
  })

  supChen.gen <- apply(boot.gen, 2, function(tt) max(abs(tt)))
  mean(supChen.gen > max(abs(t.cen)))
}



B.list <- list(B)
pval.vec <- c(1)
kk.mat <- t(combn(ncol(B),2))


while(nrow(kk.mat) > 1){
  
  print(ncol(B))
  
  Bs <- constructBs(B,kk.mat)
  pvals <- sapply(1:length(Bs), function(k){
    print(k)
    computeP(Bs[[k]])
  })
  
  win <- which.max(pvals)
  kk <- kk.mat[win,]
  B <- Bs[[win]]
  
  lose <- which(pvals < .25)
  lose <- unique(c(lose, which(apply(kk.mat,1,function(ij) any(ij == kk[2])))))
  
  kk.mat <- kk.mat[-lose,]
  kk.mat <- matrix(kk.mat - (kk.mat > kk[2]), ncol=2)

  B.list <- c(B.list,list(B))
  pval.vec <- c(pval.vec,max(pvals))
}

plot(pval.vec)

B <- B.list[[which.min(abs(pval.vec-.5))]]
clus <- c(B %*% seq(ncol(B)))

BB <- tcrossprod(B)

Tau.tilde <- (BB %*% (Tau.hat - diag(d)) %*% BB)/(BB %*% (1 - diag(d)) %*% BB)
diag(Tau.tilde) <- 1

oo <- order(clus)
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))
image(t(Tau.tilde[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))

image(t((Tau.hat-Tau.tilde)[rev(oo),oo]), col = rainbow(100))

hist((Tau.hat-Tau.tilde)[ij.mat], breaks=20)


names(clus) <- rownames(Tau.hat)
sapply(unique(clus), function(cl){
  meta[Symbol %in% names(clus[clus == cl]),cbind(Symbol,industry)]
})

