initialObjects <- computeInitialization <- function(Tau.hat, doPlot = T, method = "average"){
  # hc <- hclust(as.dist(sqrt(1-Tau.hat)), "ward.D")
  # hc <- hclust(as.dist(sqrt(1-Tau.hat)), "ward.D")
  hc <- hclust(as.dist(sqrt(1-Tau.hat)), method)
  
  if(doPlot) plot(hc)
  
  
  dend <- as.dendrogram(hc)
  true.labels <- NULL
  
  if(any(!sapply(labels(dend),is.integer))){
    true.labels <- get_leaves_attr(dend,"label")
    labels(dend) <- sapply(labels(dend), function(lab) which(colnames(Tau.hat) == lab))
  }
  
  dend <- dendrapply(dend, function(node){
    attr(node,"delta") <- -1 - !is.leaf(node)
    node
  })
  
  return(list(dend = dend, labels = true.labels))
}
