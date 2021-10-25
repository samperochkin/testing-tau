## Checks copula

library(HAC)
n <- 100
d <- 5
X <- matrix(rnorm(n*d),n,d)
family = "clayton"
departure <- "single"
tau <- .25; dtau <- .2

theta <- c(tau2theta(tau, type = 3),
           tau2theta(tau+dtau, type = 3))

if(departure == "single"){
  tree <- c(list(c(as.list(paste0(1:2)),theta[2])),c(as.list(paste0(3:d)),theta[1]))
  tree1 <- c(as.list(paste0(3:d)),theta[1])
  tree2 <- c(as.list(paste0(1:2)),theta[2])
}else if(departure == "column"){
  tree <- list("1", c(as.list(paste0(2:d)),theta[2]), theta[1])
  tree1 <- c(as.list(paste0(1)),theta[1])
  tree2 <- c(as.list(paste0(2:d)),theta[2])
} 
hhac <- hac(3,tree)
hhac1 <- hac(3,tree1)
hhac2 <- hac(3,tree2)
X <- rHAC(n, hhac)

colnames(X) <- 1:d
U <- apply(X,2,rank)/(nrow(X)+1)

####
# In fun setup

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

# extra
uus <- list(U[1,I1],U[1,I2])


if(family == "clayton"){
  
  c <- 1
  psi.s <- function(t, s) (1+t)^(-1/theta[s])
  HAC::phi(.1,theta[1],3); psi.s(.1,1)
  
  psiI.s <- function(u, s) u^(-theta[s]) - 1
  HAC::phi.inv(.1,theta[1],3); psiI.s(.1,1)
  
  #******* given, and probably good
  psiPrime.s <- function(t, s, k) kFun(-1/theta[s],k) * (1+t)^(-k-1/theta[s]) # kth derivative w.r.t. t, see later for kFun
  psiIPrime.s <- function(u, s) - theta[s] * u^(-theta[s]-1) # first derivative w.r.t. u
  #*******
  
  
  C.s <- function(uu.s, s) sapply(uu.s, psiI.s, s) %>% sum %>% psi.s(s = s)
  HAC::pHAC(uus[[1]], hhac1); C.s(uus[[1]],1)
  
  C <- function(uus) sapply(seq_along(uus), function(s) C.s(uus[[s]], s)) %>% C.s(s=1)
  HAC::pHAC(U[1,,drop=F], hhac); C(uus)

    
  # Wolframalpha checked
  # page 4
  CDot.s <- function(uu.s, s) C.s(uu.s, s) * (P4.BDot(s) * log(P4.A(uu.s, s)) + P4.B(s) * P4.ADot(uu.s, s) / P4.A(uu.s, s))
  # where
  P4.A <- function(uu.s, s) 1 + (sapply(uu.s, function(u) u^(-theta[s]) - 1) %>% sum)  
  P4.B <- function(s) -1/theta[s]
  P4.ADot <- function(uu.s, s) - sapply(uu.s, function(u) u^(-theta[s]) * log(u)) %>% sum
  P4.BDot <- function(s) 1/theta[s]^2
  
  # compare with differences **
  eps <- .0001
  ths <- seq(theta[2]-.1,theta[2]+.1,eps)
  (sapply(ths, function(th){
    theta[2] <- th
    
    psi.s <- function(t, s) (1+t)^(-1/theta[s])
    psiI.s <- function(u, s) u^(-theta[s]) - 1
    
    C.s <- function(uu.s, s) sapply(uu.s, psiI.s, s) %>% sum %>% psi.s(s = s)
    C.s(uus[[2]],2)
  }) %>% diff)[round(length(ths)/2)]/eps
  CDot.s(uus[[2]],2)
  
  # Wolframalpha checked
  psiIDot.s <- function(u, s) - u^(-theta[s]) * log(u)
  psiIPrimeDot.s <- function(u, s) u^(-theta[s]-1) * (theta[s] * log(u) - 1)
  
}




###################------------------------------------------
# General functions
###################------------------------------------------

# I TRUST THE PAPER
Qset <- function(k){
  partitions::blockparts(dd-rep(1L, d0), k-d0) + 1L
}
sFun <- function(x,n,k) sapply(k:n, function(l) copula::Stirling1(n,l) * copula::Stirling2(l,k) * x^(l)) %>% sum
sFun(.8,3,2)

# top page 4 -- derivative w.r.t. x
sFunPrime <- function(x,n,k) sum(sapply(k:n, function(l) copula::Stirling1(n,l) * copula::Stirling2(l,k) * l * x^(l-1)))

eps <- .0001
sFunPrime(.8,3,2); (sFun(.8 + eps,3,2) - sFun(.8,3,2))/eps

# top page 4 -- derivative of sFun(theta[1]/theta[2]) w.r.t. theta[s]
sFun1sDot.s <- function(s,n,k) - sFunPrime(theta[1]/theta[s],n,k) * theta[1]/theta[s]^2

eps <- .0001
ths <- seq(theta[2]-.1,theta[2]+.1,eps)
(sapply(ths, function(th) sFun(theta[1]/th,3,2)) %>% diff)[round(length(ths)/2)] / eps; sFun1sDot.s(s=2,3,2)

# okay
kFun <- function(x,k) sapply(1:k, function(j) copula::Stirling1(k,j) * x^j) %>% sum
kFun(-.8,10)

# given
a <- function(t, s, n, k) (c^theta[s] + t)^(theta[1]*k/theta[s] - n) * sFun(theta[1]/theta[s], n, k)
b <- function(tt, k){
  jj <- Qset(k)
  
  apply(jj, 2, function(j){
    prod(sapply(1:d0, function(s){
      a(tt[s], s, dd[s], j[s])
    }))
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

eps <- .00001
theta2s <- seq(theta[2]-.0001, theta[2]+.0001, eps)
theta2 <- theta[2]
ats <- rep(NA, length(theta2s))
k <- d-1; n <- dd[2]; j <- Qset(k)[2,1]
for(l in seq_along(theta2s)){
  theta[2] <- theta2s[l]
  ats[l] <- at.s(uus[[2]], 2, n, j)
}
theta[2] <- theta2
(ats %>% diff)[round(length(ats)/2)] / eps; atDot.s(uus[[2]],2,n,j)

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

eps <- .000001
theta2s <- seq(theta[2]-.0001, theta[2]+.0001, eps)
theta2 <- theta[2]
T1.ABCs <- matrix(NA, 3, length(theta2s))
for(k in seq_along(theta2s)){
  theta[2] <- theta2s[k]
  T1.ABCs[,k] <- c(T1.A(uus,d-1),T1.B(uus,d-1),T1.C(uus,d-1))
}
theta[2] <- theta2
apply(T1.ABCs,1,diff)[round(ncol(T1.ABCs)/2),] / eps
c(T1.ADot(uus,d-1),T1.BDot(uus,d-1),T1.CDot(uus,d-1))


T1Dot.num <- function(uus) sapply(d0:d, function(k) T1.ADot(uus, k)) %>% sum 
T1Dot.denum <- function(uus) sapply(d0:d, function(k) T1.A(uus, k)) %>% sum 
T1Dot <- function(uus) T1Dot.num(uus)/T1Dot.denum(uus)

eps <- .00001
theta2s <- seq(theta[2]-.0001, theta[2]+.0001, eps)
theta2 <- theta[2]
T1s <- rep(NA, length(theta2s))
for(l in seq_along(theta2s)){
  theta[2] <- theta2s[l]
  T1s[l] <- log( - sapply(d0:d, function(k) T1.A(uus, k)) %>% sum)
}
theta[2] <- theta2
(T1s %>% diff)[round(length(T1s)/2)] / eps; T1Dot(uus)


T2Dot <- function(uu.2) sapply(uu.2, function(u) psiIPrimeDot.s(u, s=2) / psiIPrime.s(u, s=2)) %>% sum 
eps <- .00001
theta2s <- seq(theta[2]-.0001, theta[2]+.0001, eps)
theta2 <- theta[2]
T2s <- rep(NA, length(theta2s))
for(l in seq_along(theta2s)){
  theta[2] <- theta2s[l]
  T2s[l] <- sapply(uus[[2]], function(u) log( - psiIPrime.s(u,2))) %>% sum
}
theta[2] <- theta2
(T2s %>% diff)[round(length(T2s)/2)] / eps; T2Dot(uus[[2]])
#------------------------------------------------------------





