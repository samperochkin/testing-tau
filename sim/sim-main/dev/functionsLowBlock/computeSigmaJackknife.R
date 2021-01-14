computeSigmaJackknife <- function(HP, ij.mat=NULL, both=F, clus=NULL){
  if(is.list(HP)) HP <- sapply(HP, function(hp) hp[ij.mat])
  
  ShJ <- cov(t(HP))
  
  if(!both) return(list(ShJ = ShJ))
  
  return(list(ShJ = ShJ, SbJ = averageSigmaBlock(ShJ, ij.mat, clus)))
}
