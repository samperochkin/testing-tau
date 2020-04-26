X <- rmvnorm(n=150, sigma = sin(pi*Tau/2))

structureBuilder <- function(X, hclust_method = "average", alpha = .05, M = 5000){
  require(pcaPP)
  
  Tau.hajek <- constructTauHajek(X)
  Tau.hat <- pcaPP::cor.fk(X)
  init <- initialObjects(Tau.hat, doPlot = F, method = hclust_method)
  
  dend <- init$dend
  
  source("thesis-application/functions/getAddresses.R")
  source("thesis-application/functions/getSubDend.R")

  vec.address <- getAddresses(dend)
  
  plot(dend)
  vv <- c(0,2,1,1)
  
  for(v in rev(vec.address[-1])){
    print(v)
    
    # if(identical(v,vv)) break
    
    if(is.leaf(dend[[v[-1]]])) next
    
    dend0 <- validify(dend,v)
    al <- testStructure(dend0,Tau.hat,Tau.hajek,M=1000)
    print(al)
    
    if(al > alpha){
      attr(dend[[v[-1]]], "delta") <- as.integer(length(dend[[v[-1]]]) == 2)
    }else{
      print(c("hey!!",al))
      dend[[v[-c(1,length(v))]]] <- unbranch(dend[[v[-c(1,length(v))]]],v[length(v)])
      attr(dend[[v[-c(1,length(v))]]],"delta") <- -2
    }
  }
  
  source("thesis-application/functions/constructTauTilde.R")
  Tau.tilde <- constructTauTilde(dend,Tau.hat)
  
  image(t(Tau.tilde[d:1,]))
  
}