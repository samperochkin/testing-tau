tryPullChildren2 <- function(dend, node, v, Tau.hat, Tau.hajek, M = 5000){
  
  # children that could be pulled
  candidates <- which(!sapply(node, is.leaf))
  candidates <- candidates[which(sapply(node[candidates], attr, which="delta")==1)]
  if(length(candidates) != 0) candidates <- candidates[order(get_childrens_heights(node)[candidates],decreasing = T)]

  for(r in seq_along(candidates)){
  
    # select the cluster formed last
    k <- candidates[r]
    al <- testInner(dend,node,k,Tau.hat,Tau.hajek,M)
  
    if(al > alpha){
      candidates[candidates > k] <- candidates[candidates > k] + length(node[[k]])
      node <- unbranch(node,k)
      attr(node,"delta") <- 1
    }else{
      break
    }
  }

return(node)
}



