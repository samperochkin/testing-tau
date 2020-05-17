initializeDend <- function(Tau.hat, method = "mcquitty", doPlot = F){
  hc <- hclust(as.dist(1-Tau.hat), method)
  
  if(doPlot) plot(hc)
  
  dend <- as.dendrogram(hc)

  dend <- dendrapply(dend, function(node){
    attr(node,"delta") <- -1 - !is.leaf(node)
    node
  })
  
  return(dend)
}
