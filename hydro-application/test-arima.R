# there could be some El Nino effect, but it is hard to conclude this given
# that there is a lot of dependence among the time series.
# Note : If arima(p,1,0) is fitted,
#         Fitting a model on residuals of arima(5,1,0) models give the same result in the end.


Y <- apply(X, 2, function(y){
  #arima(y,order = c(**SPECIFY**))$res
})  
rownames(Y) <- data2$YEAR
nrow(Y) - apply(Y,2, function(y) length(unique(y))) # no ties
plot(Y[,1])




P <- sapply(1:10, function(ll){
  Box.pvals <- apply(Y, 2,function(y) Box.test(y, lag = ll, type = c("Ljung-Box"), fitdf = pmax(0,0))$p.value)
  nn <- sapply(as.numeric(colnames(Y)), function(nn) stns[stns$ID == nn,]$`Station Name`)
  names(Box.pvals) <- sapply(1:18, function(k) paste0(nn[k]," [",k,"]"))
  Box.pvals
})


apply(P < .05, 2, sum)
apply(P < .05/18, 2, sum) # bonferroni


clus <- readRDS("data/clus.rds")


d <- ncol(Y)
p <- d*(d-1)/2
ij.mat <- cbind(unlist(sapply(1:d, function(i) seq(i)[-i])),
                unlist(sapply(2:d, function(i) rep(i,i-1))))
ij.l.mat <- matrix(0,d,d)
ij.l.mat[ij.mat] <- ij.l.mat[ij.mat[,2:1]] <- 1:nrow(ij.mat)

K <- length(unique(clus))
ds <- table(clus)
rs.mat <- cbind(unlist(sapply(1:K, function(i) seq(i)[-i])),
                unlist(sapply(2:K, function(i) rep(i,i-1))))

ps <- tcrossprod(ds)[rs.mat] # block sizes
L <- p - sum(ps) + nrow(rs.mat)


# ij.rs.mat <- matrix(clus[ij.mat], ncol=2)

B <- apply(rs.mat,1,function(rs){
  v <- rep(0,p)
  G1 <- which(clus==rs[1])
  G2 <- which(clus==rs[2])
  v[c(ij.l.mat[G1,G2])] <- 1
  v
})
B <- cbind(B,matrix(0,p,L-ncol(B)))
B[rowSums(B)==0,colSums(B)==0] <- diag(sum(rowSums(B)==0))


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


sapply(list.files("C:/Users/Samuel/Gits/testing-tau/sim-main/functionsLow2",full.names=T), source, local=environment())
library(data.table)
library(mvtnorm)
source("blockTest.R")
dt <- blockTest(Y,B,ij.mat,10000)
dt[,.(S,Sh,norm,pvalue=round(pvalue,digits=3))]


ks <- which(apply(B,2,sum) > 1)
dts <- lapply(ks, function(k){
  l <- which(B[,k] > 0)
  B2 <- matrix(0,nrow(B),nrow(B)-length(l)+1)
  B2[l,1] <- 1
  B2[-l,-1] <- diag(nrow(B)-length(l))
  df <- blockTest(Y,B2,ij.mat,10000)
  df$block <- k
  df
})


# validation of blocks ic
M <- Reduce("+",lapply(ks, function(k){
  M <- matrix(0,d,d)
  M[ij.mat][which(B[,k] > 0)] <- k/(max(ks)+1)
  M
}))
image(t(M[d:1,]))

lapply(dts, function(df){
  df[c(7,8),.(S,Sh,norm,pvalue=round(pvalue,digits=2),block)]
})
df.mat <- sapply(dts, function(df){
  df[c(7,8)]$pvalue
})
rownames(df.mat) <- dts[[1]][c(7,8)]$norm
colnames(df.mat) <- ks

library(xtable)
xtable(100*df.mat,digits=1)



# Test Intercontinental blocks

keep <- 7:14
L2 <- p - sum(B[,keep]) + 1
B2 <- matrix(0,p,L2)
B2[,1] <- rowSums(B[,keep])
B2[B2[,1] == 0,2:L2] <- diag(L2-1)
df2 <- blockTest(Y,B2,ij.mat,10000)



# Merge cluster 2 and 3, and test overall + individual

clus3 <- clus
clus3[7:d] <- clus3[7:d] - 1

K3 <- length(unique(clus3))
ds3 <- table(clus3)
rs.mat3 <- cbind(unlist(sapply(1:K3, function(i) seq(i)[-i])),
                 unlist(sapply(2:K3, function(i) rep(i,i-1))))

ps3 <- tcrossprod(ds3)[rs.mat3] # block sizes
L3 <- p - sum(ps3) + nrow(rs.mat3)


B3 <- apply(rs.mat3,1,function(rs){
  v <- rep(0,p)
  G1 <- which(clus3==rs[1])
  G2 <- which(clus3==rs[2])
  v[c(ij.l.mat[G1,G2])] <- 1
  v
})
B3 <- cbind(B3,matrix(0,p,L3-ncol(B3)))
B3[rowSums(B3)==0,colSums(B3)==0] <- diag(sum(rowSums(B3)==0))

df3 <- blockTest(Y,B3,ij.mat,10000)

ks3 <- which(apply(B3,2,sum) > 1)
dts3 <- lapply(ks3, function(k){
  l <- which(B3[,k] > 0)
  newB <- matrix(0,nrow(B3),nrow(B3)-length(l)+1)
  newB[l,1] <- 1
  newB[-l,-1] <- diag(nrow(B3)-length(l))
  df <- blockTest(Y,newB,ij.mat,10000)
  df$block <- k
  df
})
df.mat3 <- sapply(dts3, function(df){
  df[c(7,8)]$pvalue
})
rownames(df.mat3) <- dts3[[1]][c(7,8)]$norm
colnames(df.mat3) <- ks3

xtable(100*df.mat3,digits=1)
hist(df.mat3[1,], probability = T)
hist(df.mat3[2,], probability = T)

# Hierarchical
b <- which(rs.mat3[,1] %in% c(1,2) & rs.mat3[,2] %in% c(3,4,5))

L4 <- L3 - length(b) + 1
B4 <- B3
B4[,b[1]] <- rowSums(B3[,b])
B4 <- B4[,-b[-1]]
df4 <- blockTest(Y,B4,ij.mat,10000)
