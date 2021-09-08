scoreFunction <- function(X, distribution = "normal", tau, departure = NULL){
  
  if(is.vector(X)) X <- matrix(X, nrow=1)
  d <- ncol(X)
  ij.mat <- t(combn(d,2)) # ********* MAKE SURE THIS IS IT
  
  if(distribution == "normal"){
    
        R <- diag(d) + (1-diag(d))*sin(tau*pi/2)
        
        Ri <- solve(R)
        XRi <- X %*% Ri
        XRiERiX <- apply(ij.mat,1,function(ij){
          E <- matrix(0,d,d)
          E[rbind(ij,rev(ij))] <- 1
          # sum((XRi %*% E) * XRi)
          rowSums((XRi %*% E) * XRi)
        })
        
        # return(- nrow(X)*Ri[ij.mat] + XRiERiX/2)
        return(t(- Ri[ij.mat] + t(XRiERiX)/2))
  }
  
  if(distribution == "t4"){
        R <- diag(d) + (1-diag(d))*sin(tau*pi/2)
        
        Ri <- solve(R)
        XRi <- X %*% Ri
        XRiX <- rowSums(XRi * X)
        XRiERiX <- apply(ij.mat,1,function(ij){
          E <- matrix(0,d,d)
          E[rbind(ij,rev(ij))] <- 1
          # sum(((XRi %*% E) * XRi)/(4 + XRiX))
          rowSums(((XRi %*% E) * XRi)/(4 + XRiX))
        })
        # return(- nrow(X)*Ri[ij.mat] + ((d+4)/2) * XRiERiX)
        return(t(- Ri[ij.mat] + ((d+4)/2) * t(XRiERiX)))
  }

  # artificially set the first component to zero. It will not get involved
  if(distribution == "clayton") return(cbind(0,scoreFunctionTOP(X, family = "clayton", tau, departure)))
  if(distribution == "gumbel") return(cbind(0,scoreFunctionTOP(X, family = "gumbel", tau, departure)))
}

library(magrittr)
scoreFunctionTOP <- function(X, family, tau, departure){
  
  if(is.vector(X)) X <- matrix(X, nrow=1)
  
  n <- nrow(X)
  d <- ncol(X)
  if(departure == "single"){
    d1 <- d-2; d2 <- 2
    I1 <- 3:d; I2 <- 1:2
  } 
  if(departure == "column"){
    d1 <- 1; d2 <- d-1
    I1 <- 1; I2 <- 2:d
  }
  dd <- c(d1,d2)
  d0 <- length(dd)
  
  ####################-----------------------------------------
  # Specific functions
  ####################-----------------------------------------
  if(family == "clayton"){
    
    theta <- rep(tau2theta(tau,3),2)
    c <- 1
    
    psi.s <- function(t, s) (1+t)^(-1/theta[s])
    psiI.s <- function(u, s) u^(-theta[s]) - 1
    psiPrime.s <- function(t, s, k) kFun(-1/theta[s],k) * (1+t)^(-k-1/theta[s]) # kth derivative w.r.t. t, see later for kFun
    psiIPrime.s <- function(u, s) - theta[s] * u^(-theta[s]-1) # first derivative w.r.t. u
    
    C.s <- function(uu.s, s) sapply(uu.s, psiI.s, s) %>% sum %>% psi.s(s = s)
    C <- function(uus) sapply(seq_along(uus), function(s) C.s(uus[[s]], s)) %>% C.s(s=1)
    
    CDot.s <- function(uu.s, s) C.s(uu.s, s) * (P4.BDot(s) * log(P4.A(uu.s, s)) + P4.B(s) * P4.ADot(uu.s, s) / P4.A(uu.s, s))
    # where
    P4.A <- function(uu.s, s) 1 + (sapply(uu.s, function(u) u^(-theta[s]) - 1) %>% sum)  
    P4.B <- function(s) -1/theta[s]
    P4.ADot <- function(uu.s, s) - sapply(uu.s, function(u) u^(-theta[s]) * log(u)) %>% sum
    P4.BDot <- function(s) 1/theta[s]^2
    
    psiIDot.s <- function(u, s) - u^(-theta[s]) * log(u)
    psiIPrimeDot.s <- function(u, s) u^(-theta[s]-1) * (theta[s] * log(u) - 1)
  }else if(family == "gumbel"){
    
    theta <- rep(tau2theta(tau,1),2)
    c <- 0
    
    psi.s <- function(t, s) exp(-t^(1/theta[s]))
    psiI.s <- function(u, s) (-log(u))^theta[s]
    # psiPrime.s <- function(t, s, k) -exp(-t^(1/theta[s])) * t^(1/theta[s] - 1) / theta[s]
    psiPrime.s <- function(t, s, k) psi.s(t, s)/t^k * sum(sapply(1:k, function(j) sFun(1/theta[s], k, j)*(-1/theta[s])^j))
    psiIPrime.s <- function(u, s) -theta[s]/u * (-log(u))^(theta[s]-1)
    
    C.s <- function(uu.s, s) sapply(uu.s, psiI.s, s) %>% sum %>% psi.s(s = s)
    C <- function(uus) sapply(seq_along(uus), function(s) C.s(uus[[s]], s)) %>% C.s(s=1)
    
    CDot.s <- function(uu.s, s) C.s(uu.s, s) * (- P4.A(uu.s, s)^P4.B(s)) *
      (P4.BDot(s) * log(P4.A(uu.s, s)) + P4.B(s) * P4.ADot(uu.s, s) / P4.A(uu.s, s))
    # where
    P4.A <- function(uu.s, s) sapply(uu.s, function(u) (-log(u))^(theta[s])) %>% sum  
    P4.B <- function(s) 1/theta[s]
    P4.ADot <- function(uu.s, s) sapply(uu.s, function(u) (-log(u))^(theta[s]) * log(-log(u))) %>% sum
    P4.BDot <- function(s) -1/theta[s]^2
    
    psiIDot.s <- function(u, s) (-log(u))^theta[s] * log(-log(u))
    psiIPrimeDot.s <- function(u, s) - (-log(u))^(theta[s]-1) / u * (theta[s] * log(-log(u)) + 1)

  }
  
  ###################------------------------------------------
  # General functions
  ###################------------------------------------------
  Qset <- function(k){
    partitions::blockparts(dd-rep(1L, d0), k-d0) + 1L
  }
  Qset.lookup <- list()
  Qset.lookup[d0:d] <- lapply(d0:d, Qset)
  
  sFun <- function(x,n,k) sapply(k:n, function(l) stirling1.lookup[n,l] * stirling2.lookup[l,k] * x^(l)) %>% sum
  # where
  stirling1.ind <- lapply(1:(d+1), function(n) expand.grid(n=n,l=1:n)) %>% do.call(what="rbind") %>% as.matrix
  stirling2.ind <- lapply(1:(d+1), function(k) expand.grid(l=k:(d+1),k=k)) %>% do.call(what="rbind") %>% as.matrix
  stirling1.lookup <- matrix(NA, nrow=d+1, ncol=d+1)
  stirling2.lookup <- matrix(NA, nrow=d+1, ncol=d+1)
  stirling1.lookup[stirling1.ind] <- mapply(FUN = function(n,l) copula::Stirling1(n,l), stirling1.ind[,1], stirling1.ind[,2])
  stirling2.lookup[stirling2.ind] <- mapply(FUN = function(l,k) copula::Stirling2(l,k), stirling2.ind[,1], stirling2.ind[,2])
  
  # top page 4 -- derivative w.r.t. x AND derivative w.r.t. theta[s]
  sFunPrime <- function(x,n,k) sum(sapply(k:n, function(l) stirling1.lookup[n,l] * stirling2.lookup[l,k] * l * x^(l-1)))
  sFun1sDot.s <- function(s,n,k) - sFunPrime(theta[1]/theta[s],n,k) * theta[1]/theta[s]^2
  
  kFun <- function(x,k) sapply(1:k, function(j){
    stirling1.lookup[k,j] * x^j
  }) %>% sum
  
  # given
  a <- function(t, s, n, k) (c^theta[s] + t)^(theta[1]*k/theta[s] - n) * sFun(theta[1]/theta[s], n, k)
  b <- function(tt, k){
    apply(Qset.lookup[[k]], 2, function(j){
      sapply(1:d0, function(s) a(tt[s], s, dd[s], j[s]) ) %>% prod
    }) %>% sum
  }
  t.s <- function(uu.s, s) sapply(uu.s, psiI.s, s = s) %>% sum
  tt <- function(uus) sapply(seq_along(uus), function(s) t.s(uus[[s]], s=s) )  
  ttt <- function(uus) psiI.s(C(uus), s=1)
  
  at.s <- function(uu.s, s, n, k) t.s(uu.s, s=s) %>% a(s = s, n = n, k = k)
  bt <- function(uus, k) tt(uus) %>% b(k = k)
  
  ########
  # PAGE 3 (and top 4)
  ########-----------------------------------------------------
  atDot.s <- function(uu.s, s, n, k) DDot(uu.s, s, n, k) * sFun(theta[1]/theta[s], n, k) + D(uu.s, s, n, k) * sFun1sDot.s(s, n, k)
  # where
  D <- function(uu.s, s, n, k) E(uu.s, s)^G(s, n, k)
  E <- function(uu.s, s) c^theta[s] + t.s(uu.s, s=s)
  G <- function(s, n, k) theta[1]*k/theta[s] - n
  DDot <- function(uu.s, s, n, k) D(uu.s, s, n, k) * (GDot(k) * log(E(uu.s, s)) + G(s, n, k) * EDot(uu.s) / E(uu.s, s))
  if(c == 0) EDot <- function(uu.s) (sapply(uu.s, psiIDot.s, s=2) %>% sum) # depend on the specific function psiIDot.s
  if(c > 0) EDot <- function(uu.s) c^theta[2] * log(c) + (sapply(uu.s, psiIDot.s, s=2) %>% sum) # depend on the specific function psiIDot.s
  GDot <- function(k) -theta[1]/theta[2]^2 * k
  #------------------------------------------------------------
  
  ############
  # PAGE 1 - 2
  ############-------------------------------------------------
  T1.A <- function(uus, k) T1.B(uus, k) * T1.C(uus, k)
  # where
  T1.B <- function(uus, k) (tt(uus) %>% b(k = k))
  T1.C <- function(uus, k) (ttt(uus) %>% psiPrime.s(s=1, k = k))
  
  T1.ADot <- function(uus, k) T1.BDot(uus, k) * T1.C(uus, k) + T1.B(uus, k) * T1.CDot(uus, k)
  # where
  T1.BDot <- function(uus, k) apply(Qset.lookup[[k]], 2, function(jj) at.s(uus[[1]], 1, dd[1], jj[1]) * atDot.s(uus[[2]], s=2, dd[2], jj[2])) %>% sum
  T1.CDot <- function(uus, k) (ttt(uus) %>% psiPrime.s(s = 1, k = k+1)) * psiIPrime.s(C.s(uus[[2]], s=2),s=1) * CDot.s(uus[[2]], s=2)
  
  T1Dot.num <- function(uus) sapply(d0:d, function(k) T1.ADot(uus, k)) %>% sum 
  T1Dot.denum <- function(uus) sapply(d0:d, function(k) T1.A(uus, k)) %>% sum 
  T1Dot <- function(uus){
    # avoid dividing by T1Dot.denum(uus), which is pretty unstable for very small values of uus.
    # might introduce some bias
    # if(identical(T1Dot.num(uus),0)){
    #   print("Hit a bad run in scoreFunctionTOP")
    #   return(0)
    # } 
    T1Dot.num(uus)/T1Dot.denum(uus)
  }
  
  T2Dot <- function(uu.2) sapply(uu.2, function(u) psiIPrimeDot.s(u, s=2) / psiIPrime.s(u, s=2)) %>% sum 
  #------------------------------------------------------------
  
  ###########################
  # Putting it all together #
  ###########################
  # U <- apply(X,2,rank)/(nrow(X)+1) #********** Is this necessary? **********#
  
  # lookup tables for atDot.s (within the apply calls below) would be the next step to improve speed.
  
  # T1Dot.eval <- apply(X,1,function(uu) T1Dot(list(uu[I1],uu[I2]))) %>% sum
  # T2Dot.eval <- apply(X[,I2,drop=F],1,T2Dot) %>% sum
  T1Dot.eval <- lapply(1:nrow(X), function(k) T1Dot(list(X[k,I1],X[k,I2]))) %>% unlist
  T2Dot.eval <- apply(X[,I2,drop=F],1,T2Dot)
  T1Dot.eval + T2Dot.eval
}
