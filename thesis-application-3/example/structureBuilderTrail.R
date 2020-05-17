structureBuilderTrail <- function(X, hclust_method = "average", alpha = .05, M = 5000){
  
  library(pcaPP)
  library(dendextend)
  
  invisible(sapply(list.files("thesis-application-3/functions/", full.names = T), source, local = environment()))
  # source("thesis-application-3/side-functions/testInner-conditional.R", local = environment())
  
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
  for(v in rev(vec.address)){
    node <- getSubDend(dend,v)
    node[1:length(node)] <- node[order(sapply(node,function(nn) min(unlist(nn))))]
    dend <- assignSubDend(node,dend,v)
  }
  vec.address <- getAddresses(dend)
  
  v.list <- list()
  dend.begin.list <- list()
  res.vec <- integer(0)
  Tau.list <- list()
  
  dend.modifs.list <- list()
  v.modifs.list <- list()
  res.modifs.list <- list()
  Tau.modifs.list <- list()
  
  ll <- length(vec.address)
  
  print("Loop")
  for(s in rev(seq_along(vec.address))){
    v <- vec.address[[s]]

    v.list[[ll-s+1]] <- v
    dend.begin.list[[ll-s+1]] <- dend
    Tau.list[[ll-s+1]] <- constructTauTilde(dend,Tau.hat)
    print(v)
    
    node <- getSubDend(dend,v)
    if(is.leaf(node)) next
    # if(length(node) == 3) break
    
    al <- testOuter(dend,v,Tau.hat,Tau.hajek,M=M)
    
    if(al > alpha){ # SET DELTA AND TRY PULL UP
      
      res.vec <- c(res.vec,1)
      
      delta <- as.integer(length(node) == 2)
      attr(node, "delta") <- delta
      
      dend.modifs.list[[ll-s+1]] <- list()
      v.modifs.list[[ll-s+1]] <- integer(0)
      res.modifs.list[[ll-s+1]] <- integer(0)
      Tau.modifs.list[[ll-s+1]] <- list()
      
      # children that could be pulled
      candidates <- which(!sapply(node, is.leaf))
      candidates <- candidates[which(sapply(node[candidates], attr, which="delta")==1)]
      if(delta == 1 & length(candidates) != 0){
        candidates <- candidates[order(get_childrens_heights(node)[candidates],decreasing = T)]
        for(r in seq_along(candidates)){
          
          dend.modifs.list[[ll-s+1]] <- c(dend.modifs.list[[ll-s+1]],list(assignSubDend(node,dend,v)))
          v.modifs.list[[ll-s+1]] <- c(v.modifs.list[[ll-s+1]], r)
          Tau.modifs.list[[ll-s+1]] <- c(Tau.modifs.list[[ll-s+1]],list(constructTauTilde(assignSubDend(node,dend,v),Tau.hat)))
          
          # select the cluster formed last
          k <- candidates[r]
          al <- testInner(node,k,Tau.hat,Tau.hajek,M)
          res.modifs.list[[ll-s+1]] <- c(res.modifs.list[[ll-s+1]], as.integer(al > alpha))
          
          if(al > alpha){
            candidates[candidates > k] <- candidates[candidates > k] + length(node[[k]]) - 1
            node <- unbranch(node,k)
            attr(node,"delta") <- 1
          }else{
            break
          }
        }
        
        v.modifs.list[[ll-s+1]] <- candidates[v.modifs.list[[ll-s+1]]]
      }
      dend <- assignSubDend(node,dend,v)
      vec.address <- getAddresses(dend)
      
    }else{ # BREAK STRUCTURE AND PUSH UP
      
      res.vec <- c(res.vec, 0)
      
      parent <- getSubDend(dend, v[-length(v)])
      parent <- unbranch(parent,v[length(v)])
      attr(parent,"delta") <- -2
      dend <- assignSubDend(parent,dend,v[-length(v)])
      vec.address <- getAddresses(dend)
    }
    
    
    
    # plot(dend)
  }
  
  # attr(dend,"delta") <- as.integer(length(dend) > 2)
  Tau.tilde <- constructTauTilde(dend,Tau.hat)
  labels(dend) <- true.labels[as.numeric(labels(dend))]
  
  list(dend = dend, Tau.tilde = Tau.tilde,
       v.list = v.list,
       dend.begin.list = dend.begin.list,
       res.vec = res.vec,
       Tau.list = Tau.list,
       dend.modifs.list = dend.modifs.list,
       v.modifs.list = v.modifs.list,
       res.modifs.list = res.modifs.list,
       Tau.modifs.list = Tau.modifs.list
  )
}

