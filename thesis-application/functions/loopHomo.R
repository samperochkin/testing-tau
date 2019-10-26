loopHomo <- function(dend){
  
  vec.address <- getAddresses(dend)
  
  
  for(v in rev(vec.address)){
    
    # upload node
    node <- getSubDend(dend,v)

    # trivial cases
    if(attr(node,"members") <= 2) next
    if(attr(node,"type") == 0) next
    
    while(T){
      
      # children that could be pulled
      candidates <- which(!sapply(node, is.leaf))
      candidates <- candidates[which(sapply(node[candidates], attr, which="type")==1)]
      
      if(length(candidates) == 0) break
      
      # select the cluster formed last
      k <- candidates[which.max(get_childrens_heights(node)[candidates])]
      
      # select the cluster formed last
      pval <- testInner(Tau.hat,node,k)
      
      if(pval > .05){
        node <- unbranch(node,k)
        attr(node,"type") <- 1
      }else{
        break
      }
    }
    
    dend <- assignSubDend(node,dend,v)
  }
  
  return(dend)
}