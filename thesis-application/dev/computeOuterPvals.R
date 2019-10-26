computeOuterPvals <- function(dend,alpha){
  dendrapply(dend, function(node){
    
    cond <- attr(node,"Opval") > alpha
    if(identical(cond,T) | attr(node,"members") %in% c(1,d)) return(node)
    # if(attr(node,"members") %in% c(1,d)) return(node)
    
    attr(node,"Opval") <- testOuter(Tau.hat,get_leaves_attr(node,"label"))
    node
    
  })
}
