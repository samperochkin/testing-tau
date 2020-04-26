correctNode <- function(node){
  
  to.pull <- which(!sapply(node, is.leaf))
  to.pull <- to.pull[which(!sapply(node[to.pull], attr, which="valid"))]
  
  for(k in rev(to.pull)){
    node <- unbranch(node,k)
    attr(node, "type") <- 0
  }
  
  return(node)
}