getSubDend <- function(dend,v){
  if(length(v) == 1){
    return(dend)
  }else{
    return(dend[[v[-1]]])
  }
}