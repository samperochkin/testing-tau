source("thesis-application/setup/big-structure.R")
# source("thesis-application/small-structure.R")
# source("thesis-application/setup/tiny-structure.R")
source("thesis-application/setup/generate.R")
source("thesis-application/setup/setup.R")

library(dendextend)
sapply(list.files("thesis-application/functions/",full.names = T), source)


# library(dendextend)
hc <- hclust(as.dist(sqrt(pmax(1-Tau.hat,0))), "single")
par(mfrow=c(1,1))
plot(hc)

dend <- as.dendrogram(hc)
dend <- dendrapply(dend, function(node){
  if(!is.leaf(node)) attr(node,"type") <- 1
  node
})
dend <- computeOuterPvals(dend,.05)
Opvals <- get_nodes_attr(dend,"Opval")

while(any(Opvals < .05, na.rm=T)){
  print("new pass")
  vec.address <- getAddresses(dend)
  print("breaking")
  dend <- breakProblems(dend,vec.address,.05)
  print("computing")
  dend <- computeOuterPvals(dend,.05)
  Opvals <- get_nodes_attr(dend,"Opval")
}

plot(dend)
plot(Opvals, ylim=c(0,1))

dend <- tryMergeHomo(dend)
plot(dend, ylim=c(0,1))

dend <- constructBs(dend)
Tau.tilde <- buildTauTilde(dend, Tau.hat)

par(mfrow = c(1,3), mar = c(1,1,1,1))
image(t(Tau[d:1,]), zlim = c(0,1))
image(t(Tau.hat[d:1,]), zlim = c(0,1))
image(t(Tau.tilde[d:1,]), zlim = c(0,1))

sum((Tau-Tau.tilde)^2)/sum((Tau-Tau.hat)^2)