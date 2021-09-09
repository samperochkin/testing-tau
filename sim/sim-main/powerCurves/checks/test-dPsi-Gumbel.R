library(copula)
library(HAC)
library(magrittr)
tau <- .3

#### gumbel ####
theta <- tau2theta(tau,1)
c <- 0

psi.s <- function(t) exp(-t^(1/theta))

psiPrime1 <- function(t, k){
  psi.s(t)/t^k * 
    sapply(1:k, function(j){
      (-1/theta)^j * sapply(j:k, function(l) Stirling1(k,l) * Stirling2(l,j) * (1/theta)^l ) %>% sum()
    }) %>% sum()
}

psiPrime2 <- function(t, k){
  (-1)^k * psi.s(t) / t^k *
    sapply(1:k, function(j){
      # (1/theta)^j *
      t^(j/theta)
      factorial(k)/factorial(j) *
        (sapply(1:j, function(l) choose(j,l) * choose(l/theta,k) * (-1)^(k-l) ) %>% sum())
    }) %>% sum()
}
# psiPrime2 <- function(t, k){
#   (-1)^k * psi.s(t)/t^k *
#     sapply(1:k, function(j){
#       # (1/theta)^j * (-1)^(k-j) *
#       t^(j/theta) * (-1)^(k-j) *
#         sapply(j:k, function(l) (1/theta)^l * Stirling1(k,l) * Stirling2(l,j) ) %>% sum()
#     }) %>% sum()
# }

psiPrime1(.1, 5); psiPrime2(.1, 5)
psiPrime1(.1, 6); psiPrime2(.1, 6)
psiPrime1(.1, 7); psiPrime2(.1, 7)

psiPrime1(1.1, 5); psiPrime2(1.1, 5)
psiPrime1(1.1, 6); psiPrime2(1.1, 6)
psiPrime1(1.1, 7); psiPrime2(1.1, 7)

psiPrime1(1, 25); psiPrime2(1, 25)
psiPrime1(.001, 25); psiPrime2(.001, 25)
psiPrime1(100, 25); psiPrime2(100, 25)

psiPrime1(1e4, 7); psiPrime2(1e4, 7)
psiPrime1(1e5, 7); psiPrime2(1e5, 7)
psiPrime1(1e4, 3); psiPrime2(1e4, 3)
psiPrime1(.0000001, 7); psiPrime2(.0000001, 7)

system.time(replicate(1000,psiPrime1(.001, 7)))
system.time(replicate(1000,psiPrime2(.001, 7)))

step_size <- .00001
diff(psi.s(seq(1,1+2*step_size,step_size)))/step_size
psiPrime1(1+step_size, 1); psiPrime2(1+step_size, 1)

diff(diff(psi.s(seq(1,1+2*step_size,step_size)))/step_size)/step_size
psiPrime1(1+step_size, 2); psiPrime2(1+step_size, 2)
