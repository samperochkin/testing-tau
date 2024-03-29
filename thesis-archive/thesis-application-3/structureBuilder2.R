structureBuilder <- function(X, hclust_method = "mcquitty", alpha0 = .05, alpha1 = .1, M = 5000){
  
  library(pcaPP)
  library(dendextend)
  
  invisible(sapply(list.files("thesis-application-3/double/functions/", full.names = T), source, local = environment()))


  # labelling stuff
  true.labels <- colnames(X)
  if(is.null(true.labels)) true.labels <- 1:ncol(X)
  colnames(X) <- 1:ncol(X)
  
  print("Init")
  # initialization
  Tau.hat <- pcaPP::cor.fk(X)
  Tau.hajek <- constructTauHajek(X)
  
  dend <- initializeDend(Tau.hat, method = hclust_method, doPlot=F)
  vec.address <- getAddresses(dend)

  
  print("Loop")
  for(s in rev(seq_along(vec.address))){
    print(s)
    v <- vec.address[[s]]

    node <- getSubDend(dend,v)
    if(is.leaf(node)) next
    
    al <- testOuter(dend,v,Tau.hat,Tau.hajek,M=M)
    
    if(al > alpha0){ # SET DELTA AND TRY PULL UP
      
      delta <- as.integer(length(node) == 2)
      attr(node, "delta") <- delta
      
      if(delta == 1){
        # if(length(unlist(node)) == 4) break
        node <- tryPullChildren(node, Tau.hat, Tau.hajek, M = M, alpha1 = alpha1)
      }
      dend <- assignSubDend(node,dend,v)
      vec.address <- getAddresses(dend)
      
    }else{ # BREAK STRUCTURE AND PUSH UP
      
      parent <- getSubDend(dend, v[-length(v)])
      parent <- unbranch(parent,v[length(v)])
      attr(parent,"delta") <- -2
      dend <- assignSubDend(parent,dend,v[-length(v)])
      vec.address <- getAddresses(dend)
    }
    
    # plot(dend)
  }
  
  Tau.tilde <- constructTauTilde(dend,Tau.hat)
  labels(dend) <- true.labels[as.numeric(labels(dend))]
  
  list(dend = dend, Tau.tilde = Tau.tilde)
}
