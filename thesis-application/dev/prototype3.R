source("thesis-application/setup/big-structure.R")
# source("thesis-application/setup/small-structure.R")
# source("thesis-application/setup/tiny-structure.R")
source("thesis-application/setup/generate.R")
source("thesis-application/setup/setup.R")

library(dendextend)
sapply(list.files("thesis-application/functions/",full.names = T), source)

alpha <- .05

hc <- hclust(as.dist(sqrt(pmax(1-Tau.hat,0))), "single")
par(mfrow=c(1,1))
plot(hc)

dend <- as.dendrogram(hc)
dend <- dendrapply(dend, function(node){
  if(!is.leaf(node)) attr(node,"type") <- 1
  node
})
vec.address <- getAddresses(dend)

for(v in rev(vec.address)){
  # upload node
  if(length(v) == 1){
    node <- dend
  }else{
    node <- dend[[v[-1]]]
  }
  
  if(attr(node,"members") <= 2) next
  
  to.break <- NA
  while(length(to.break) > 0){
    pvals <- sapply(node, testOuter, Tau.hat = Tau.hat)
    to.break <- which(pvals < alpha)
    
    for(k in rev(to.break)){
      node <- unbranch(node,k)
      attr(node, "type") <- 0
    }
  }
  
  # re-assign node
  if(length(v) == 1){
    dend <- node
  }else{
    dend[[v[-1]]] <- node
  }
}

plot(dend)






vec.address <- getAddresses(dend)
for(v in rev(vec.address)){
  
  # upload node
  if(length(v) == 1){
    node <- dend
  }else{
    node <- dend[[v[-1]]]
  }
  
  if(attr(node,"members") <= 2) next
  if(attr(node,"type") == 0) next
  
  while(T){
    
    candidates <- which(!sapply(node, is.leaf))
    candidates <- candidates[which(sapply(node[candidates], attr, which="type")==1)]
    
    if(length(candidates) == 0) break
    
    k <- candidates[which.max(get_childrens_heights(node)[candidates])]
    pval <- testInner(Tau.hat,node,k)

    if(pval > .05){
      node <- unbranch(node,k)
      attr(node,"type") <- 1
    }else{
      break
    }
  }
  
  # re-assign node
  if(length(v) == 1){
    dend <- node
  }else{
    dend[[v[-1]]] <- node
  }
}




dend <- constructBs(dend)
Tau.tilde <- buildTauTilde(dend, Tau.hat)

par(mfrow = c(1,3), mar = c(1,1,1,1))
image(t(Tau[d:1,]), zlim = c(0,1))
image(t(Tau.hat[d:1,]), zlim = c(0,1))
image(t(Tau.tilde[d:1,]), zlim = c(0,1))

sum((Tau-Tau.tilde)^2)/sum((Tau-Tau.hat)^2)
