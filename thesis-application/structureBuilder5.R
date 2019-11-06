structureBuilder <- function(X, M = 2000, alpha = .05){
  
  print(c(M,alpha))

  print("Setup")
  library(dendextend)
  sapply(list.files("thesis-application/functions/", full.names = T), source, local = environment())
  source("thesis-application/setup/setup.R", local = environment())
  
  Tau.hat <- cor.fk(X)
  

  print("Initialization")
  dend.init <- initialObjects(Tau.hat)$dend
  Tau.init <- constructTauTilde(dend.init)
  
  
  print("Loop - hetero")
  dend.hetero <- loopHetero(dend.init)
  Tau.hetero <- constructTauTilde(dend.hetero)
  
  print("Loop - homo")
  dend.final <- loopHomo(dend.hetero)
  Tau.final <- constructTauTilde(dend.final)
  
  dends <- list(dend.init,dend.hetero,dend.final)
  Taus <- list(Tau.hat,Tau.init,Tau.hetero,Tau.final)
  
  return(list(dends = dends, Taus = Taus))
}
