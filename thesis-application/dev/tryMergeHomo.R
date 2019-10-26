tryMergeHomo <- function(dend){
  
  vec.address <- list(0)
  
  k <- 1
  while(k <= length(vec.address)){

    #### GET NODE
    v <- vec.address[[k]]
    if(length(v) == 1){
      node <- dend
    }else{
      node <- dend[[v[-1]]]
    }
    
    #### IF LEAF, NOTHING
    if(is.leaf(node)){
      k <- k + 1
      next
    } 
    
    if(identical(attr(node,"type"),0)){
      #### next addresses
      vec.address <- c(vec.address, lapply(1:length(node), function(s) c(v,s)))
      #### counter update
      k <- k + 1
      next
    }
    
    
    #### ELSE FIND HIGHEST CHILD AND TRY MERGING
    r <- which.max(get_childrens_heights(node))
    still <- identical(attr(node[[r]],"type"),1)
    
    if(!still){
      k <- k + 1
      next
    } 
    
    while(still){
      Ipval <- testInner(Tau.hat,node,r)
      if(Ipval > .05){
        node <- unbranch(node,r)
        r <- which.max(get_childrens_heights(node))
        still <- identical(attr(node[[r]],"type"),1)
      }else{
        still <- FALSE
      } 
    }
    
    attr(node,"type") <- 1
    
    #### REASSIGN
    if(length(v) == 1){
      dend <- node
    }else{
      dend[[v[-1]]] <- node
    }
    
    #### next addresses
    vec.address <- c(vec.address, lapply(1:length(node), function(s) c(v,s)))
    #### counter update
    k <- k + 1
  }
  
  return(dend)
}

