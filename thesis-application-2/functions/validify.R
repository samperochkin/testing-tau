validify <- function(dend, w = NULL){
  
  dend0 <- dend
  
  attr(dend0[[w[-1]]], "delta") <- as.integer(length(dend0[[w[-1]]]) == 2)
  
  deltas <- get_root_branches_attr(dend0,"delta")
  
  while(any(deltas == -2)){
    ks <- which(deltas == -2)
    
    for(k in rev(ks)){
      dend0 <- unbranch(dend0,k)
    }
    
    deltas <- get_root_branches_attr(dend0,"delta")
  }

  attr(dend0,"delta") <- 0
  return(dend0)
}
