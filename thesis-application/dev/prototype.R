source("thesis-application/big-structure.R")
# source("thesis-application/small-structure.R")
source("thesis-application/generate.R")
source("thesis-application/setup.R")

task <- list(1:d)
Bs <- list()
types <- integer(0)
max.it <- 10  

while(length(task) > 0){
  print(task[[1]])
  
  i.ind <- task[[1]]
  l.ind <- ij.l.mat[t(combn(i.ind,2))]
  d2 <- length(i.ind)
  
  if(d2 == 2){
    B <- matrix(0,d,1)
    B[i.ind,] <- 1
    Bs[[length(Bs)+1]] <- B
    types <- c(types,1)
  }else if(d2 > 2){
    hc <- hclust(as.dist(sqrt(pmax(1-Tau.hat[i.ind,i.ind],0))), "single")
    #plot(hc)
    
    clus <- cutree(hc,2)
    B <- matrix(0,d2,2)
    B[cbind(1:d2,clus)] <- 1
    
    pvals <- testOuter(Tau.hat,B,i.ind,l.ind)
    
    if(pvals > .05){
      k <- 2
      while(pvals[k-1] > .05 & k <= (d2-1)){
        k <- k + 1
        print(k)
        clus <- cutree(hc,k)
        B <- matrix(0,d2,k)
        B[cbind(1:d2,clus)] <- 1
        pvals <- c(pvals,testOuter2(Tau.hat,B,i.ind,l.ind)) 
      }
    }else{
      k <- 2
      while(pvals[k-1] < .5 & k <= (d2-1) & (k - which.min(abs(pvals-.5)) < max.it)){
        k <- k + 1
        print(k)
        clus <- cutree(hc,k)
        B <- matrix(0,d2,k)
        B[cbind(1:d2,clus)] <- 1
        pvals <- c(pvals,testOuter(Tau.hat,B,i.ind,l.ind)) 
      }
      pvals <- cummax(pvals)
    } 
    
    if(pvals[1] > .05){
      K <- max(which(rev(pvals > .5)[1]),which.min(abs(pvals-.5))) + 1
    }else{
      K <- which.min(abs(pvals-.5)+(pvals<.05)) + 1
    }
    
    clus <- cutree(hc,K)
    B <- matrix(0,d,K)
    B[cbind(i.ind,clus)] <- 1
    Bs[[length(Bs)+1]] <- B
    types <- c(types,pvals[1] > .05)
    
    new.task <- lapply(1:K, function(kk) i.ind[which(clus == kk)])
    task <- c(task, new.task[sapply(new.task,length) > 1])
  }
  task <- task[-1]
}    

Tau.tilde <- Tau.hat
for(k in seq_along(Bs)){
  B <- Bs[[k]]
  type <- types[k]
  BB <- tcrossprod(B)

  Tb <- (BB %*% (Tau.hat - diag(d)) %*% t(BB)) / (BB %*% (1 - diag(d)) %*% t(BB))
  if(type == 1){
    
    Tau.tilde[BB == 0 & !(is.nan(Tb))] <- mean(Tb[BB == 0  & !(is.nan(Tb))])
  }else{
    Tau.tilde[BB == 0 & !(is.nan(Tb))] <- Tb[BB == 0  & !(is.nan(Tb))]
  }
  diag(Tau.tilde) <- 1
}


par(mfrow = c(1,3), mar = c(1,1,1,1))
image(t(Tau[d:1,]), zlim = c(0,1))
image(t(Tau.hat[d:1,]), zlim = c(0,1))
image(t(Tau.tilde[d:1,]), zlim = c(0,1))


