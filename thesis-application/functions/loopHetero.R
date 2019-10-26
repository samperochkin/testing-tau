loopHetero <- function(dend){
  
  vec.address <- getAddresses(dend)
  
  for(v in rev(vec.address)){
    print(v)
    node <- getSubDend(dend,v)
    
    if(is.leaf(node)) next
    
    node <- correctNode(node)
    dend <- assignSubDend(node,dend,v)
    
    if(!identical(v,0)){
      attr(node, "valid") <- testOuter(node, Tau.hat, dend) > alpha
      dend <- assignSubDend(node,dend,v)
    }
  }
  
  return(dend)
}