getAddresses <- function(dend){
  vec.address <- list(0)
  k <- 1
  while(k <= length(vec.address)){
    v <- vec.address[[k]]
    if(length(v) == 1){
      node <- dend
    }else{
      node <- dend[[v[-1]]]
    }
    
    if(!is.leaf(node)){
      vec.address <- c(vec.address, lapply(1:length(node), function(k) c(v,k)))
    }
    
    k <- k + 1
  }  
  vec.address
}
