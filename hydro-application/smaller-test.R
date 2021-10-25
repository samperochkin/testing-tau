
# pre-processed data
Y <- readRDS("data/Y.rds")
clus <- readRDS("data/clus.rds")

Y <- Y[,2:8]
clus <- clus[2:8] - 1
d <- ncol(Y)
p <- d*(d-1)/2
ij.mat <- cbind(unlist(sapply(1:d, function(i) seq(i)[-i])),
                unlist(sapply(2:d, function(i) rep(i,i-1))))


K <- length(unique(clus))
ds <- table(clus)
rs.mat <- cbind(unlist(sapply(1:K, function(i) seq(i)[-i])),
                unlist(sapply(2:K, function(i) rep(i,i-1))))

ps <- tcrossprod(ds)[rs.mat] # block sizes
L <- p - sum(ps) + nrow(rs.mat)


ij.rs.mat <- matrix(clus[ij.mat], ncol=2)

B <- t(apply(ij.rs.mat,1,function(rs){
  v <- rep(0,L)
  if(rs[1] == rs[2]) return(v)
  v[cumsum(c(0,K:2 - 1))[rs[1]] + (rs[2] - rs[1])] <- 1
  v
}))
B[cbind(which(rowSums(B)==0),(nrow(rs.mat)+1):L)] <- 1
colSums(B)


par(mfrow = c(1,2), mar = c(1,1,1,1))
pal11 <- colorRampPalette(c("green3","lightgray","indianred4"))
pal12 <- colorRampPalette(2:7)
pal <- c(pal11(100),pal12(50))


Tau.hat <- pcaPP::cor.fk(Y)
tau.hat <- Tau.hat[ij.mat]
BBp <- tcrossprod(B)
BBp <- BBp/rowSums(BBp)
tau.tilde <- BBp %*% tau.hat
Tau.tilde <- diag(d)
Tau.tilde[rbind(ij.mat,ij.mat[,2:1])] <- tau.tilde

ss <- seq(1.0001,2,length.out = 6)
diag(Tau.hat) <- unlist(sapply(1:K, function(k) rep(ss[k],ds[k])))
diag(Tau.tilde) <- unlist(sapply(1:K, function(k) rep(ss[k],ds[k])))

par(mar=rep(0,4))
# image(t(Tau.hat[d:1,]), zlim=c(-1,2), col = c(pal(100),"black"), xaxt="n", yaxt="n")
image(t(Tau.hat[d:1,]), zlim=c(-1,2), col = pal, xaxt="n", yaxt="n")


eps <- 1/(d-1)
ss <- seq(-eps/2,1+eps/2,length.out=d+1)
rs.mat <- cbind(unlist(sapply(1:K, function(i) seq(i)[-i])),
                unlist(sapply(2:K, function(i) rep(i,i-1))))
cds <- c(0,cumsum(ds)) + 1
pal2 <- colorRampPalette(c("violet","purple3"))

par(mar=rep(0,4))
image(t(Tau.tilde[d:1,]), zlim=c(-1,2), col = pal, xaxt="n", yaxt="n")
sapply(1:nrow(rs.mat),function(k){
  r <- rs.mat[k,1]
  s <- rs.mat[k,2]
  
  rect(ss[cds[s]],ss[d-cds[r]+2],ss[cds[s+1]],ss[d-cds[r+1]+2])
  # ,
  #  col = pal2(nrow(rs.mat))[k])
  
  x <- (ss[cds[s]]+ss[cds[s+1]])/2
  y <- (ss[d-cds[r]+2]+ss[d-cds[r+1]+2])/2
  
  text(x,y,labels=k)
})




# The test ----------------------------------------------------------------

# source("functions/testStructure.R")
# testStructure(Y,B,ij.mat,10000)


sapply(list.files("C:/Users/Samuel/Dropbox/Git Projects/testing-tau/sim-main/functionsLow2/",full.names=T), source, local=environment())
library(data.table)
library(mvtnorm)
dt <- blockTest(Y,B,ij.mat,10000)

round(dt$pvalue,digits=3)
