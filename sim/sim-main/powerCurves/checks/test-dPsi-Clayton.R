library(copula)
library(HAC)
library(magrittr)
tau <- .3

#### clayton ####
theta <- tau2theta(tau,3)
c <- 1

psi.s <- function(t) (1+t)^(-1/theta)

psiPrime1 <- function(t, k){
  (sapply(1:k, function(j){
    Stirling1(k,j) * (-1/theta)^j
  }) %>% sum()) * 
    (1 + t)^(-k-1/theta)
}

psiPrime2 <- function(t, k){
  (-1)^k * gamma(k + 1/theta)/gamma(1/theta) * (1+t)^(-k-1/theta)
}

psiPrime1(.1, 5); psiPrime2(.1, 5)
psiPrime1(.1, 6); psiPrime2(.1, 6)
psiPrime1(.1, 7); psiPrime2(.1, 7)

psiPrime1(1.1, 5); psiPrime2(1.1, 5)
psiPrime1(1.1, 6); psiPrime2(1.1, 6)
psiPrime1(1.1, 7); psiPrime2(1.1, 7)

psiPrime1(1e8, 7); psiPrime2(1e8, 7)
psiPrime1(.0000001, 7); psiPrime2(.0000001, 7)

system.time(replicate(1000,psiPrime1(.001, 7)))
system.time(replicate(1000,psiPrime2(.001, 7)))
