assignSubDend <- function(node,dend,v){
  if(length(v) == 1){
    dend <- node
  }else{
    dend[[v[-1]]] <- node
  }
  
  return(dend)
}