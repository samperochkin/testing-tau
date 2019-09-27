library(data.table)
library(pcaPP)
library(Matrix)
library(mvtnorm)

source("sim-low-dim/functions/testEquiRank.R")
source("sim-low-dim/functions/computeTh.R")
source("sim-low-dim/functions/buildSigma.R")

source("sim-high-dim/functions/testEquiRankSim.R")
source("sim-high-dim/functions/jackknifeVariance.R")

X <- readRDS("application/returns_mat.rds")
#X <- X[nrow(X) + (-51:0),]
meta <- fread("application/NASDAQ100_meta.csv")


table(meta[,Sector])
sectors <- meta[,unique(Sector)]
nn <- meta[Sector == "Technology" , unlist(Symbol)]
# X <- X[,nn]
X <- X[,nn[1:25]]

n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
ij.l.mat <- matrix(NA,d,d)
ij.l.mat[ij.mat] <- ij.l.mat[ij.mat[,2:1]] <- 1:p

Tau.hat <- cor.fk(X)
t.hat <- Tau.hat[ij.mat]

# Boot
C.gen.raw <- sapply(1:n, function(r){
  V <- t(X[r,] < t(X[-r,]))
  rowSums(apply(V, 1, function(v){
    4*(outer(v,v,"=="))[ij.mat]
  }))/(n*(n-1)) - t.hat/n
})


constructBs <- function(B,kk.mat){
  lapply(1:nrow(kk.mat), function(kk){
    kk <- kk.mat[kk,]
    B[,kk[1]] <- rowSums(B[,kk])
    B[,-kk[2]]
  })
}

# B.old <- B
# B.new <- Bs[[k]]
# boot.gen <- M.list[[1]]
# G.master <- M.list[[2]]

updateBoot <- function(B.new, B.old, boot.gen, G.master){
  
  tiime0 <- Sys.time()
  
  D <- crossprod(B.old,B.new)
  k <- which(colSums(D > 0) > 1)
  ind <- which(B.new[,k]==1)
  
  r <- unique(c(ij.l.mat[ind,]))
  r <- sort(r[!is.na(r)])
  
  tiime00 <- Sys.time()
  K <- ncol(B.new)
  clus <- c(B.new %*% seq(K))
  
  block <- matrix(clus[ij.mat[r,]],ncol=2)
  block <- apply(block, 1, sort)
  
  block <-  c(c(K,1) %*% (block - c(1,0)))
  G <- matrix(0,nrow=length(r),ncol=max(block))
  G[cbind(1:length(r),block)] <- 1
  ss <- colSums(G)
  if(any(ss == 0)) G <- G[,-which(ss==0)]
  G <- tcrossprod(G)
  G <- G/rowSums(G)
  tiime1 <- Sys.time()
  
  boot.gen[r,] <- (diag(length(r))-G) %*% C.gen.noise[r,]
  G.master[r,r] <- G 
  list(boot.gen,G.master)
}

computeP <- function(M){
  # print(max(abs((diag(p)-M[[2]]) %*% t.hat)))
  supChen.gen <- apply(M[[1]], 2, function(tt) max(abs(tt)))
  mean(supChen.gen > max(abs((diag(p)-M[[2]]) %*% t.hat)))
}





B <- diag(d)
B.list <- list(B)
pval.vec <- c(1)

M <- 2000
C.gen.noise <- (C.gen.raw %*% matrix(rnorm(n*M),n,M))
M.list <- list(matrix(0,p,M),diag(p))



for(al in c(.05,.1,.2,.3,.4,.5)){
  kk.mat <- t(combn(ncol(B),2))
  while(nrow(kk.mat) > 1){
    
    print(ncol(B))
    
    Bs <- constructBs(B,kk.mat)
    pvals <- sapply(1:length(Bs), function(k){
      if(k %% 10 == 0) print(k)
      ML <- updateBoot(Bs[[k]],B,M.list[[1]],M.list[[2]])
      computeP(ML)
    })
    
    win <- which.max(pvals)
    M.list  <- updateBoot(Bs[[win]],B,M.list[[1]],M.list[[2]])
    kk <- kk.mat[win,]
    B <- Bs[[win]]
    
    lose <- which(pvals < al) # HERE
    lose <- unique(c(lose, which(apply(kk.mat,1,function(ij) any(ij == kk[2])))))
    
    kk.mat <- kk.mat[-lose,]
    kk.mat <- matrix(kk.mat - (kk.mat > kk[2]), ncol=2)
    
    B.list <- c(B.list,list(B))
    pval.vec <- c(pval.vec,max(pvals))
  }
}


plot(pval.vec)
hist(pval.vec)



# B <- B.list[[4]]
# B <- B.list[[which.min(abs(pval.vec-.5))]]
B <- B.list[[which.min(abs(pval.vec-.49))]]
clus <- c(B %*% seq(ncol(B)))
names(clus) <- colnames(Tau.hat)
clus

BB <- tcrossprod(B)

Tau.tilde <- (BB %*% (Tau.hat - diag(d)) %*% BB)/(BB %*% (1 - diag(d)) %*% BB)
diag(Tau.tilde) <- 1

oo <- order(clus)
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))
image(t(Tau.tilde[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))

image(t((Tau.hat-Tau.tilde)[rev(oo),oo]), col = rainbow(100),zlim=c(-1,1))
hist((Tau.hat-Tau.tilde)[ij.mat], breaks=20)

Tt <- (t(B) %*% (Tau.hat - diag(d)) %*% B)/(t(B) %*% (1 - diag(d)) %*% B)
Tt[is.nan(Tt)] <- 1

hc <- hclust(as.dist(1-Tt))
ooo <- hc$order
clus <- sapply(clus, function(i) which(ooo==i))

oo <- order(clus)
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))
image(t(Tau.tilde[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))
image(t((Tau.hat-Tau.tilde)[rev(oo),oo]), col = rainbow(100),zlim=c(-1,1))


names(clus) <- rownames(Tau.hat)
sapply(unique(clus), function(cl){
  meta[Symbol %in% names(clus[clus == cl]),cbind(Symbol,industry)]
})

