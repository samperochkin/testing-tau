# MIGHT NEED TO CHANGE PATH TO hydro-application/data/...


# pre-processed data
Y <- readRDS("data/Y.rds")
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


ss <- seq(-1/(2*(d-1)),1+1/(2*(d-1)),length.out=d+1)
ss2 <- ss[c(1,2,7,9,10,18,19)]

rs.mat <- cbind(unlist(sapply(1:K, function(i) seq(i)[-i])),
                unlist(sapply(2:K, function(i) rep(i,i-1))))


pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/Taus-block-3.pdf", width = 8, height = 4)
par(mfrow = c(1,2), mar=c(1,1,1,1))
image(t(Tau.hat[d:1,]), zlim=c(-.1,1), axes = F)
sapply(1:nrow(rs.mat),function(k){
  r <- rs.mat[k,1]
  s <- rs.mat[k,2]
  
  rect(ss2[s],1-ss2[r],ss2[s+1],1-ss2[r+1])
  
  # x <- (ss2[s]+ss2[s+1])/2
  # y <- (1-ss2[r]+1-ss2[r+1])/2
  # 
  # text(x,y,labels=k)
})

# abline(h=1-ss2, lty=2)
# abline(v=ss2, lty=2)
image(t(Tau.tilde[d:1,]), zlim=c(-.1,1), axes = F)
sapply(1:nrow(rs.mat),function(k){
  r <- rs.mat[k,1]
  s <- rs.mat[k,2]
  
  rect(ss2[s],1-ss2[r],ss2[s+1],1-ss2[r+1])

  # x <- (ss2[s]+ss2[s+1])/2
  # y <- (1-ss2[r]+1-ss2[r+1])/2
  # 
  # text(x,y,labels=k)
})

dev.off()

pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/Taus-block-3-id.pdf", width = 8, height = 4)
par(mfrow = c(1,2), mar=c(1,1,1,1))
image(t(Tau.hat[d:1,]), zlim=c(-.1,1), axes = F)
sapply(1:nrow(rs.mat),function(k){
  r <- rs.mat[k,1]
  s <- rs.mat[k,2]
  
  rect(ss2[s],1-ss2[r],ss2[s+1],1-ss2[r+1])
  
  x <- (ss2[s]+ss2[s+1])/2
  y <- (1-ss2[r]+1-ss2[r+1])/2

  text(x,y,labels=k)
})

# abline(h=1-ss2, lty=2)
# abline(v=ss2, lty=2)
image(t(Tau.tilde[d:1,]), zlim=c(-.1,1), axes = F)
sapply(1:nrow(rs.mat),function(k){
  r <- rs.mat[k,1]
  s <- rs.mat[k,2]
  
  rect(ss2[s],1-ss2[r],ss2[s+1],1-ss2[r+1])
  
  x <- (ss2[s]+ss2[s+1])/2
  y <- (1-ss2[r]+1-ss2[r+1])/2

  text(x,y,labels=k)
})

dev.off()