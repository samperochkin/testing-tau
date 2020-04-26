changeDelta <- function(dend,v,delta){
  node <- getSubDend(dend,v)
  attr(node,"delta") <- delta
  assignSubDend(node,dend,v)
}
