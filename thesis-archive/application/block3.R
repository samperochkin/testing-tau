# While loop. As soon as you hit near the middle, you merge.
# cutree sur single en montant -- expand.grid des deux groupes

B <- diag(d)
B.list <- list(B)
pval.vec <- c(.5)

M <- 1000
C.gen.noise <- (C.gen.raw %*% matrix(rnorm(n*M),n,M))
mmfun <- function(tt){max(abs(tt))}
# mm <- apply(C.gen.noise, 2, mmfun)
m <- 0
mm <- rep(0,M)


for(radius in c(.05,.1,.15,.2,.3,.4,.5)){
  K <- 2
  while(K <= ncol(B)){
    
    pvals <- sapply(1:(K-1), function(k){
      B2 <- B[,-K,drop=F]
      B2[,k] <- rowSums(B[,c(k,K)])
      
      ind <- which(B2[,k]==1)
      
      r <- unique(c(ij.l.mat[ind,]))
      r <- sort(r[!is.na(r)])
      
      tiime00 <- Sys.time()
      clus <- c(B2 %*% seq(ncol(B2)))
      
      block <- matrix(clus[ij.mat[r,]],ncol=2)
      block <- apply(block, 1, sort)
      
      block <-  c(c(ncol(B2),1) %*% (block - c(1,0)))
      G <- matrix(0,nrow=length(r),ncol=max(block))
      G[cbind(1:length(r),block)] <- 1
      ss <- colSums(G)
      if(any(ss == 0)) G <- G[,-which(ss==0)]
      G <- tcrossprod(G)
      G <- G/rowSums(G)
      tiime1 <- Sys.time()
      
      mm <- pmax(mm,
                 apply((diag(length(r))-G) %*% C.gen.noise[r,], 2, mmfun))
      m <- max(m, (diag(length(r))-G) %*% t.hat[r])
      mean(mm > m)
    })
    pvals
    
    if(any(pvals %between% (.5 + c(-1,0)*radius + c(0,.5)))){
      win <- which.max(abs(pvals))
      B2 <- B[,-K,drop=F]
      B2[,win] <- rowSums(B[,c(win,K)])
      B <- B2
      B.list <- c(B.list,list(B))
      pval.vec <- c(pval.vec,pvals[win])
    }else{
      K <- K+1
    }
  }
}

plot(pval.vec)

win <- rev(which(pval.vec > .5))[1]
B0 <- B.list[[win]]

clus <- c(B0 %*% seq(ncol(B0)))
names(clus) <- colnames(Tau.hat)
clus

BB <- tcrossprod(B0)

Tau.tilde <- (BB %*% (Tau.hat - diag(d)) %*% BB)/(BB %*% (1 - diag(d)) %*% BB)
diag(Tau.tilde) <- 1

oo <- order(clus)
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))
image(t(Tau.tilde[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))

image(t((Tau.hat-Tau.tilde)[rev(oo),oo]), col = rainbow(100),zlim=c(-1,1))
hist(sqrt(n)*(Tau.hat-Tau.tilde)[ij.mat], breaks=20, probability = T)
lines(seq(-4,4,.02),dnorm(seq(-4,4,.02)),col="red")

Tt <- (t(B0) %*% (Tau.hat - diag(d)) %*% B0)/(t(B0) %*% (1 - diag(d)) %*% B0)
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


pdf("application/figures/T_hat.pdf", width = 3, height = 3)
par(mar=c(0,0,0,0))
image(t(Tau.hat[rev(1:d),]), col = rainbow(100), zlim = c(0,1))
dev.off()
pdf("application/figures/T_hat2.pdf", width = 3, height = 3)
par(mar=c(0,0,0,0))
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100), zlim = c(0,1))
dev.off()
pdf("application/figures/T_tilde.pdf", width = 3, height = 3)
par(mar=c(0,0,0,0))
image(t(Tau.tilde[rev(oo),oo]), col = rainbow(100), zlim = c(0,1))
dev.off()
pdf("application/figures/T_res.pdf", width = 3, height = 3)
par(mar=c(0,0,0,0))
image(t((Tau.hat-Tau.tilde)[rev(oo),oo]), col = rainbow(100))
dev.off()


