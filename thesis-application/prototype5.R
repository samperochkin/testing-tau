source("thesis-application/setup/constructFun.R")
# source("thesis-application/setup/big-structure.R")
# source("thesis-application/setup/small-structure.R")
source("thesis-application/setup/tiny-structure.R")
source("thesis-application/setup/generate.R")
# X <- readRDS("application/returns_mat.rds")
# Tau.hat <- cor.fk(X)

source("thesis-application/setup/setup.R")

library(dendextend)
sapply(list.files("thesis-application/functions/",full.names = T), source)

M <- 2000
alpha <- .05
par(mfrow = c(1,1), mar = c(2,2,1,1))
dend <- initialObjects(Tau.hat)$dend

dend <- loopHetero(dend)
plot(dend)

dend <- loopHomo(dend)
plot(dend)

Tau.tilde <- constructTauTilde(dend)

# levelplot(t(Tau.hat[rev(oo),oo]), col=heat.colors(100))
# levelplot(t(Tau.tilde[rev(oo),oo]), col=heat.colors(100))

par(mfrow = c(1,3), mar = c(1,1,1,1))
image(t(Tau[d:1,]), col=heat.colors(100))
image(t(Tau.hat[d:1,]), col=heat.colors(100))
image(t(Tau.tilde[d:1,]), col=heat.colors(100))
sum((Tau-Tau.tilde)^2)/sum((Tau-Tau.hat)^2)


# oo <- unlist(dend)
# par(mfrow = c(1,2), mar = c(1,1,1,1))
# # image(t(Tau.hat[rev(oo),oo]), zlim = c(0,1), col=rainbow(100))
# # image(t(Tau.tilde[rev(oo),oo]), zlim = c(0,1), col=rainbow(100))
# image(t(Tau.hat[rev(oo),oo]), zlim = c(0,1), col=heat.colors(100))
# image(t(Tau.tilde[rev(oo),oo]), zlim = c(0,1), col=heat.colors(100))



# true.labels <- colnames(Tau.hat)[as.numeric(labels(dend))]
# # true.labels <- colnames(Tau.hat)[hc$order]
# library(data.table)
# meta <- fread("application/NASDAQ100_meta.csv")
# 
# library("ape")
# hc <- hclust(as.dist(1-Tau.hat), "average")
# hc <- hclust(as.dist(sqrt(1-Tau.tilde)), "single")
# 
# hc$height
# 
# labels(hc) <- true.labels
# plot(as.phylo(hc), type = "unrooted", cex = 0.6, no.margin = TRUE,edge.color = rainbow(15))
# 
# plot(as.phylo(hc), type = "fan", cex = 0.6, no.margin = TRUE)
# plot(as.phylo(hc), type = "radial", cex = 0.6, no.margin = TRUE)
# 
# 
# library(dendextend)
library(circlize)
# 
dend <- dendrapply(dend, function(node){
  if(is.leaf(node)) return(node)
  
  inds <- lapply(node, get_leaves_attr, attribute="label")
  
  kks <- t(combn(length(inds),2))
  
  attr(node, "height") <- 1 - mean(apply(kks,1,function(kk) mean(Tau.hat[inds[[kk[1]]],inds[[kk[2]]]])))
  node
})

circlize_dendrogram(dend)
# 
# 
# par(mfrow = c(1,1), mar = c(4,2,1,1))
# labels(dend) <- sapply(true.labels, function(nn) meta[Symbol == nn,]$Symbol)
# plot(dend)
# # 
# # 
# par(mfrow = c(1,1), mar = c(6,2,1,1))
# labels(dend) <- sapply(true.labels, function(nn) meta[Symbol == nn,]$Sector)
# plot(dend)
# # 
# # 
# par(mfrow = c(1,1), mar = c(15,2,1,1))
# labels(dend) <- sapply(true.labels, function(nn) meta[Symbol == nn,]$industry)
# plot(dend)
# # 
# # 
# par(mfrow = c(1,1), mar = c(15,2,1,1))
# labels(dend) <- sapply(true.labels, function(nn) meta[Symbol == nn,]$Name)
# plot(dend)
# 
# 
# labels(hc) <- sapply(true.labels, function(nn) meta[Symbol == nn,]$industry)
# plot(as.phylo(hc), type = "unrooted", cex = 0.6, no.margin = TRUE)
