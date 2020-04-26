getAddresses <- function(dend){
  vec.address <- list(0)
  heights <- attr(dend,"height")
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
      heights <- c(heights,get_root_branches_attr(node,"height"))
    }
    
    k <- k + 1
  }  
  vec.address[order(heights,decreasing = T)]
}
