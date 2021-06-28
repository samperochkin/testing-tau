generateData <- function(n, d, tau, dtau, dtau_type, distribution){
  
  if(is.null(dtau_type)) dtau_type <- "none"
  
  # generate data
  if(distribution == "normal"){
    
    Sig <- diag(d) + (1-diag(d))*sin(tau*pi/2)
    
    if(dtau_type == "single") Sig[cbind(c(1,2),c(2,1))] <- sin((tau + dtau)*pi/2)
    if(dtau_type == "column") Sig[-1,-1] <- diag(d-1) + (1-diag(d-1))*sin((tau + dtau)*pi/2) 
    
    X <- rmvnorm(n,rep(0,d),Sig)
    
  } else if(distribution == "cauchy"){
    
    Sig <- diag(d) + (1-diag(d))*sin(tau*pi/2)
    
    if(dtau_type == "single") Sig[cbind(c(1,2),c(2,1))] <- sin((tau + dtau)*pi/2)
    if(dtau_type == "column") Sig[-1,-1] <- diag(d-1) + (1-diag(d-1))*sin((tau + dtau)*pi/2) 
    
    X <- rmvt(n,Sig,1)
    
  }else if(distribution == "t4"){
    
    Sig <- diag(d) + (1-diag(d))*sin(tau*pi/2)
    
    if(dtau_type == "single") Sig[cbind(c(1,2),c(2,1))] <- sin((tau + dtau)*pi/2)
    if(dtau_type == "column") Sig[-1,-1] <- diag(d-1) + (1-diag(d-1))*sin((tau + dtau)*pi/2) 
    
    X <- rmvt(n,Sig,4)
    
  }else if(distribution == "joe"){
    
    if(dtau == 0){
      X <- rHAC(n, hac(7,c(as.list(as.character(1:d)),tau2theta(tau,7))))
    }else if(dtau_type == "single"){
      tree <- c(list(c(as.list(paste0(1:2)),tau2theta(tau+dtau,7))),c(as.list(paste0(3:d)),tau2theta(tau,7)))
      X <- rHAC(n, hac(7,tree))
    }else if(dtau_type == "column"){
      tree <- list("1", c(as.list(paste0(2:d)),tau2theta(tau+dtau,7)), tau2theta(tau,7))
      X <- rHAC(n, hac(7,tree))
    }
    
  }else if(distribution == "clayton"){
    
    if(dtau == 0){
      X <- rHAC(n, hac(3,c(as.list(as.character(1:d)),tau2theta(tau,3))))
    }else if(dtau_type == "single"){
      tree <- c(list(c(as.list(paste0(1:2)),tau2theta(tau+dtau,3))),c(as.list(paste0(3:d)),tau2theta(tau,3)))
      X <- rHAC(n, hac(3,tree))
    }else if(dtau_type == "column"){
      tree <- list("1", c(as.list(paste0(2:d)),tau2theta(tau+dtau,3)), tau2theta(tau,3))
      X <- rHAC(n, hac(3,tree))
    }
    
  }else if(distribution == "frank"){
    
    if(dtau == 0){
      X <- rHAC(n, hac(5,c(as.list(as.character(1:d)),tau2theta(tau,5))))
    }else if(dtau_type == "single"){
      tree <- c(list(c(as.list(paste0(1:2)),tau2theta(tau+dtau,5))),c(as.list(paste0(3:d)),tau2theta(tau,5)))
      X <- rHAC(n, hac(5,tree))
    }else if(dtau_type == "column"){
      tree <- list("1", c(as.list(paste0(2:d)),tau2theta(tau+dtau,5)), tau2theta(tau,5))
      X <- rHAC(n, hac(5,tree))
    }
  }
  return(X)
}