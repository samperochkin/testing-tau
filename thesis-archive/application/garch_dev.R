library("rugarch")

spec = ugarchspec()
fits <- apply(X, 2, ugarchfit, spec = spec)

U <- sapply(fits, function(ff) as.vector(residuals(ff)))
U <- do.call("cbind",U)



Tau.hat <- cor.fk(U)
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
rownames(D) <- colnames(D) <- colnames(U)

hc <- hclust(as.dist(D), "ward.D2")
oo <- hc$order

plot(hc)
par(mfrow=c(1,1))
image(t(Tau.hat[rev(oo),oo]), zlim = c(-1,1), col = rainbow(100))
image(t(Tau.hat[rev(oo),oo]), col = rainbow(100))

clus <- cutree(hc,36)
lapply(unique(clus), function(k){
  # hc$labels[which(clus == k)]
  meta[Symbol %in% hc$labels[which(clus == k)], Name]
  # NASDAQ100_meta[Symbol %in% hc$labels[which(clus == k)], industry]
})





