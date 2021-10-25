correctNode2 <- function(node){
  
  to.pull <- which(!sapply(node, is.leaf))
  
  cond <- unlist(sapply(node[to.pull], attr, which="valid"))
  if(is.null(cond)) cond <- numeric(0)
  
  to.pull <- to.pull[which(!cond)]
  
  for(k in rev(to.pull)){
    node <- unbranch(node,k)
    attr(node, "type") <- 0
  }
  
  return(node)
}