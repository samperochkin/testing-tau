n=100
d=4
tau=0
epsilon=.5
distribution="normal"
num.sim = 2

# packages
library(parallel)
library(mvtnorm)
library(data.table)
sapply(list.files("sim/sim-main/functionsLocal2/", full.names = T), source, local = environment())

# create a parameter grid
sim.grid <- createGrid(n, d, tau, epsilon, distribution, num.sim)
sim.grid$M <- 2500

r <- 1
# assign parameters -- just for clarity
n <- sim.grid[r,]$n
d <- sim.grid[r,]$d
p <- choose(d,2)
tau <- sim.grid[r,]$tau
epsilon <- sim.grid[r,]$dtau
dtau_type <- sim.grid[r,]$dtau_type
distribution <- sim.grid[r,]$distribution
M <- sim.grid[r,]$M

# Note that we generate data the same way as for the "regular" simulation study
X <- generateData(n,d,tau,epsilon/sqrt(n),dtau_type,distribution)



###########################################################################
# Original script computing (among other things) the epsilon vector -------
###########################################################################
# Taken from performTestsAlternativeOracle.R

n <- nrow(X)
d <- ncol(X)
p <- d*(d-1)/2

ij.mat <- t(combn(d,2))
l.mat <- matrix(0,d,d)
l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:p

Tau.hat <- pcaPP::cor.fk(X)
tau.hat <- Tau.hat[ij.mat]

# Hajek projection
HP <- computeHajekProjection(X,ij.mat)
Sigma.hats <- computeSigmaJackknife(HP, ij.mat, T, l.mat)

B <- rep(1,p)
IBB <- diag(p) - matrix(1/p,p,p)

tt <- sqrt(n)*c(IBB %*% tau.hat)
loE <- c(crossprod(tt))
loM <- max(abs(tt))


res1 <- lapply(names(Sigma.hats), function(nn){
  
  Sh <- Sigma.hats[[nn]]
  try(eig <- eigen(Sh, symmetric = T),silent=T)
  if(exists("eig")){
    
    keep <- which(eig$values > 1e-6)
    Sh2 <- eig$vectors[,keep] %*% diag(sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
    Shi <- eig$vectors[,keep] %*% diag(1/eig$values[keep]) %*% t(eig$vectors[,keep])
    Shi2 <- eig$vectors[,keep] %*% diag(1/sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
    
    # S = I ------------------------------------------------
    SI.star2 <- IBB %*% Sh2
    
    if(dtau_type == "single"){
      epsilon.vec <- epsilon/p * c(p-1,rep(-1,p-1))
    }
    if(dtau_type == "column"){
      s1 <- p; s2 <- d-1
      # epsilon <- -epsilon for the column departure
      epsilon.vec <- -epsilon/p^2 *
        c(rep( (d-1)*2*(s1-s2) + (p-d+1)*(s1-2*s2), d-1),
          rep( (d-1)*(s1-2*s2) + (p-d+1)*(-2)*s2, p-d+1))
    }
    
    # S = Sh ------------------------------------------------
    G <- matrix(colSums(Shi),p,p,byrow=T)/sum(Shi)
    IG <- diag(p) - G
    
    SSh.star <- Shi2 %*% IG %*% Sh2
    
    # bias term
    if(dtau_type == "single"){
      s1 <- sum(Shi); s2 <- sum(Shi[,1])
      P <- matrix(0,p,p)
      P[1,1] <- 2*(s1 - s2)
      P[-1,1] <- P[1,-1] <- s1 - 2*s2
      P[-1,-1] <- -2*s2
      P <- epsilon/s1^2 * P %*% Shi
    }
    if(dtau_type == "column"){
      s1 <- sum(Shi); s2 <- sum(Shi[,1:(d-1)])
      P <- matrix(0,p,p)
      P[1:(d-1),1:(d-1)] <- 2*(s1 - s2)
      P[-(1:(d-1)),(1:(d-1))] <- P[(1:(d-1)),-(1:(d-1))] <- s1 - 2*s2
      P[-(1:(d-1)),-(1:(d-1))] <- -2*s2
      # epsilon <- -epsilon for the column departure
      P <- -epsilon/s1^2 * P %*% Shi
    }
    
    # same here: we use tau directly, but Sh is estimated
    epsilon.vec2 <- Shi2 %*% rowSums(P)
    

    return(cbind(epsilon.vec, epsilon.vec2))
  }
})
res1

  
###########################################################################
# Comparison of original and new formulas for beta=0 ----------------------
###########################################################################


# Function computing both
computeG <- function(S, Shi, Shi2 = NULL, epsilon, dtau_type){
  
  p <- ncol(Shi)
  d <- ceiling(sqrt(p*2))
  
  if(S == "I"){
    Si <- Si2 <- diag(p)
  }else if(S == "Sh"){
    Si <- Shi
    Si2 <- Shi2
  }
  
  # bias term
  if(dtau_type == "single"){
    a <- sum(Si); b1 <- sum(Si[,1]); b0 <- sum(Si[1,1])
    
    P1 <- matrix(0,p,p)
    P1[1,1] <- 2*(a - b1)
    P1[-1,1] <- P1[1,-1] <- a - 2*b1
    P1[-1,-1] <- -2*b1
    P1 <- epsilon/a^2 * P1 %*% Si
    eps1 <- Si2 %*% rowSums(P1)
    
    P0 <- matrix(0,p,p)
    P0[1,1] <- a
    P0 <- P0 - b0
    P0 <- (P0/(a*b0)) %*% Si
    eps0 <- Si2 %*% P0 %*% c(1,rep(0,p-1)) * epsilon
  }
  if(dtau_type == "column"){
    a <- sum(Si); b1 <- sum(Si[,1:(d-1)]); b0 <- sum(Si[1:(d-1),1:(d-1)])
    
    P1 <- matrix(0,p,p)
    P1[1:(d-1),1:(d-1)] <- 2*(a - b)
    P1[-(1:(d-1)),(1:(d-1))] <- P1[(1:(d-1)),-(1:(d-1))] <- a - 2*b
    P1[-(1:(d-1)),-(1:(d-1))] <- -2*b
    # epsilon <- -epsilon for the column departure
    P1 <- -epsilon/a^2 * P1 %*% Si
    eps1 <- Si2 %*% rowSums(P1)
    
    P0 <- matrix(0,p,p)
    P0[1:(d-1),1:(d-1)] <- a
    P0 <- P0 - b0
    P0 <- (P0/(a*b0)) %*% Si
    # epsilon <- -epsilon for the column departure
    eps0 <- Si2 %*% P0 %*% c(rep(1,d-1),rep(0,p-d+1)) * (-epsilon)
  }

  return(cbind(eps0,eps1))
}
  
  
# Same script as above, but calling the computeG function  
res2 <- lapply(names(Sigma.hats), function(nn){
  
  Sh <- Sigma.hats[[nn]]
  try(eig <- eigen(Sh, symmetric = T),silent=T)

  keep <- which(eig$values > 1e-6)
  Sh2 <- eig$vectors[,keep] %*% diag(sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
  Shi <- eig$vectors[,keep] %*% diag(1/eig$values[keep]) %*% t(eig$vectors[,keep])
  Shi2 <- eig$vectors[,keep] %*% diag(1/sqrt(eig$values[keep])) %*% t(eig$vectors[,keep])
  
  
  return(cbind(computeG("I", Shi, Shi2, epsilon, dtau_type), computeG("Sh", Shi, Shi2, epsilon, dtau_type)))
})

# Computations for original formulas works
# Case S=I
res1[[1]]
res2[[1]][,c(2,4)]

# Case S=Sh
res1[[2]]
res2[[2]][,c(2,4)]

# And results are identical for old and new formulas
res2[[1]][,c(1,2)]
res2[[1]][,c(3,4)]
res2[[2]][,c(1,2)]
res2[[2]][,c(3,4)]
