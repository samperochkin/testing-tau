

source("sim-main/devSetup.R")
M <- 5000
B <- rep(1,p)
IBB <- diag(p) - matrix(1/p,p,p)

tab <- do.call(rbind, lapply(c("ShP","ShJ","SbP","SbJ"), function(nn){
  
  Sh <- Sigma.hats[[gsub("b","h",nn)]]
  if(grepl("b",nn)){
    Sh <- averageSigma(Sh,l.mat,full=T)
  }
  
  SI.star <- IBB %*% Sh %*% IBB
  isPD <- matrixcalc::is.positive.definite(Sh)
  
  if(isPD){
    eig <- eigen(Sh)
    Shi <- eig$vectors %*% diag(1/eig$values) %*% t(eig$vectors)
    Shi2 <- eig$vectors %*% diag(1/sqrt(eig$values)) %*% t(eig$vectors)
    
    IG <- diag(p)- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
    SSh.star <- diag(p)-tcrossprod(rowSums(Shi2))/sum(Shi)
  }
  
  resTable <- data.table(S = rep(c("Sh","I"), each=2),
                         Sh = nn,
                         norm = rep(c("Euclidean","Supremum"), times=2),
                         loss = as.numeric(NA),
                         pvalue_method = c("Direct",rep("Monte Carlo",3)),
                         pvalue = as.numeric(NA),
                         isShPd = isPD)
  
  
  if(isPD){
    tt <- sqrt(n)*c(Shi2 %*% IG %*% tau.hat)
    
    #### E -- S=Sh -- Direct approximation
    lo <- c(crossprod(tt))
    pv <- pchisq(lo,p-d,lower.tail=F)
    resTable[1, c("loss","pvalue") := list(lo,pv)]
    
    #### M -- S=Sh -- Direct approximation
    lo <- max(abs(tt))
    pv <- performMC(lo,SSh.star,"Supremum",M)
    resTable[2, c("loss","pvalue") := list(lo,pv)]
  }

  tt <- sqrt(n)*c(IBB %*% tau.hat)
  
  #### E -- S=I -- Direct approximation
  lo <- c(crossprod(tt))
  pv <- performMC(lo,SI.star,"Euclidean",M)
  resTable[3, c("loss","pvalue") := list(lo,pv)]
  
  #### M -- S=I -- Direct approximation
  lo <- max(abs(tt))
  pv <- performMC(lo,SI.star,"Supremum",M)
  resTable[4, c("loss","pvalue") := list(lo,pv)]
  
  return(resTable)
}))

