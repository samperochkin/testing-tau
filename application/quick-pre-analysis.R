library(pcaPP)

X <- readRDS("application/returns_mat.rds")
meta <- fread("application/NASDAQ100_meta.csv")


Tau.hat <- cor.fk(X)
d <- ncol(Tau.hat)
image(t(Tau.hat[d:1,]), col = rainbow(100))




# quick visu --------------------------------------------------------------

D0 <- apply(combn(d,2), 2, function(ij){
  # sum((Tau.hat[ij[1],-ij] - Tau.hat[ij[2],-ij])^2*colMeans(abs(Tau.hat[ij,-ij])))
  sum((Tau.hat[ij[1],-ij] - Tau.hat[ij[2],-ij])^2/(1-abs(colMeans(Tau.hat[ij,-ij]))))
})

D <- matrix(0,d,d)
D[t(combn(d,2))] <- D0
D <- D + t(D)
image(t(D[d:1,]), col = rainbow(100))
rownames(D) <- colnames(D) <- colnames(X)

hc <- hclust(as.dist(D), "ward.D2")
oo <- hc$order

plot(hc)
image(t(Tau.hat[rev(oo),oo]), zlim = c(-1,1), col = rainbow(100))
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100))

clus <- cutree(hc,36)
lapply(unique(clus), function(k){
  # hc$labels[which(clus == k)]
  meta[Symbol %in% hc$labels[which(clus == k)], Name]
  # NASDAQ100_meta[Symbol %in% hc$labels[which(clus == k)], industry]
})

















hc <- hclust(as.dist(sqrt(1-Tau.hat)), "average")
# hc$labels <- NASDAQ100_meta[,industry]
clus <- cutree(hc,30)
oo <- order(clus)

plot(hc)
image(t(Tau.hat[rev(oo),oo]), zlim = c(-1,1), col = rainbow(100))
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100))
image(t(sqrt(1-Tau.hat)[rev(oo),oo]), col = rainbow(100))

lapply(unique(clus), function(k){
  # hc$labels[which(clus == k)]
  NASDAQ100_meta[Symbol %in% hc$labels[which(clus == k)], Name]
  # NASDAQ100_meta[Symbol %in% hc$labels[which(clus == k)], industry]
})


# clus <- kmeans(t(apply(returns,2,rank)), centers = 10)$cluster
clus <- kmeans(t(returns), centers = 18)$cluster
oo <- order(clus)
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100))

lapply(unique(clus), function(k){
  NASDAQ100_meta[Symbol %in% names(which(clus == k)), "Name"]
})



clus <- cluster::pam(t(returns), k = 20)$clust
oo <- order(clus)
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100))

lapply(unique(clus), function(k){
  NASDAQ100_meta[Symbol %in% names(which(clus == k)), "Name"]
})

