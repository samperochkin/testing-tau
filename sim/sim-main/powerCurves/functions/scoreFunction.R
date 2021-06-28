scoreFunction <- function(distribution = "normal", params){
  #params <- list(R, ij.mat)
  
  if(distribution == "normal"){
    
    return(
      
      function(X, params){
        #params <- list(R, ij.mat)
        d <- ncol(X)
        
        Ri <- solve(params$R)
        XRi <- X %*% Ri
        XRiERiX <- apply(params$ij.mat,1,function(ij){
          E <- matrix(0,d,d)
          E[rbind(ij,rev(ij))] <- 1
          sum((XRi %*% E) * XRi)
        })
        - nrow(X)*Ri[ij.mat] + XRiERiX/2
      }
      
    )
  }
  
  if(distribution == "t4"){
    return(
      
      function(X, params){
        #params <- list(R, ij.mat)
        d <- ncol(X)
        
        Ri <- solve(params$R)
        XRi <- X %*% Ri
        XRiX <- rowSums(XRi * X)
        XRiERiX <- apply(params$ij.mat,1,function(ij){
          E <- matrix(0,d,d)
          E[rbind(ij,rev(ij))] <- 1
          sum(((XRi %*% E) * XRi)/(4 + XRiX))
        })
        - nrow(X)*Ri[params$ij.mat] + ((d+4)/2) * XRiERiX        }
      
    )
  }

  if(distribution == "clayton") return(scoreFunctionTOP(X, family = "clayton", params))
  if(distribution == "gumbel") return(scoreFunctionTOP(X, family = "gumbel", params))
}


X <- matrix(rnorm(7*10),10,7)



scoreFunctionTOP <- function(X, family, params){
  
  theta <- params$theta
  
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
    
  }
  
  ###################------------------------------------------
  # General functions
  ###################------------------------------------------
  Qset <- function(k){
    partitions::blockparts(dd-rep(1L, d0), k-d0) + 1L
  }
  sFun <- function(x,n,k) sapply(k:n, function(l) copula::Stirling1(n,l) * copula::Stirling2(l,k) * x^(l)) %>% sum
  sFun(.8,3,2)
  # top page 4 -- derivative w.r.t. x AND derivative w.r.t. theta[s]
  sFunPrime <- function(x,n,k) sum(sapply(k:n, function(l) copula::Stirling1(n,l) * copula::Stirling2(l,k) * l * x^(l-1)))
  sFun1sDot.s <- function(s,n,k) - sFunPrime(theta[1]/theta[s],n,k) * theta[1]/theta[s]^2
  
  kFun <- function(x,k) sapply(1:k, function(j) copula::Stirling1(k,j) * x^j) %>% sum
  kFun(-.8,10)
  
  # given
  a <- function(t, s, n, k) (c^theta[s] + t)^(theta[1]*k/theta[s] - n) * sFun(theta[1]/theta[s], n, k)
  b <- function(tt, k){
    apply(Qset(k), 2, function(j){
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
  EDot <- function(uu.s) c^theta[2] * log(c) + (sapply(uu.s, psiIDot.s, s=2) %>% sum) # depend on the specific function psiIDot.s
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
  T1.BDot <- function(uus, k) apply(Qset(k), 2, function(jj) at.s(uus[[1]], 1, dd[1], jj[1]) * atDot.s(uus[[2]], s=2, dd[2], jj[2])) %>% sum
  T1.CDot <- function(uus, k) (ttt(uus) %>% psiPrime.s(s = 1, k = k+1)) * psiIPrime.s(C.s(uus[[2]], s=2),s=1) * CDot.s(uus[[2]], s=2)
  
  T1Dot.num <- function(uus) sapply(d0:d, function(k) T1.ADot(uus, k)) %>% sum 
  T1Dot.denum <- function(uus) sapply(d0:d, function(k) T1.A(uus, k)) %>% sum 
  T1Dot <- function(uus) T1Dot.num(uus)/T1Dot.denum(uus)
  
  T2Dot <- function(uu.2) sapply(uu.2, function(u) psiIPrimeDot.s(u, s=2) / psiIPrime.s(u, s=2)) %>% sum 
  #------------------------------------------------------------
  
  ###########################
  # Putting it all together #
  ###########################
  U <- apply(X,2,rank)/(nrow(X)+1) #********** Is this necessary? **********#
  
  T1Dot.eval <- apply(U,1,function(uu) T1Dot(list(uu[I1],uu[I2]))) %>% sum
  T2Dot.eval <- apply(U[,I2],1,T2Dot) %>% sum
  T1Dot.eval + T2Dot.eval
}