constructBs <- function(dend){
  dendrapply(dend, function(node){
    if(is.leaf(node)) return(node)
    
    inds <- lapply(node, get_leaves_attr, attribute="label")  
    
    B <- matrix(0,d,length(inds))
    for(i in seq_along(inds)){
      B[inds[[i]],i] <- 1
    }
    attr(node, "B") <- B
    node
  })
}