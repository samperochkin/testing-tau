

# Packages ----------------------------------------------------------------

library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(parallel)

# Setup -------------------------------------------------------------------


#*************** IMPORTANT TUNING PARAMETER
large.n <- 10000 # for asymptotic results..
epsilon.vec <- seq(0,10,.1)


################
# distribution #
################
# d <- c(5,15,25)
d <- 5
# tau <- c(0,.25,.5,.75)
tau <- .25
# distribution <- c("normal","cauchy","clayton")
distribution <- c("normal")

distribution.grid <- as.data.table(expand.grid(distribution=distribution, tau=tau, d=d))
distribution.grid[,distribution_id := 1:nrow(distribution.grid)]


#############
# departure #
#############
# LATER: epsilon <- seq(0,10,.1)
dtau_type <- c("single", "column")


###################
# test statistics #
###################
# Sh <- c("ShJ", "SbJ")
Sh <- "ShJ"
S <- c("I", "Sh")
norm = c("Euclidean", "Supremum")

stat.grid <- as.data.table(expand.grid(S=S, norm=norm))
stat.grid[,stat_id := 1:nrow(stat.grid)]


# Estimation of Sigma (under H0) ------------------------------------------
source("sim/sim-main/functionsLow2/generateData.R")
source("sim/sim-main/functionsLow2/averageSigma.R")
N <- 50000

# Note that we compute only "ShJ" here, not yet generalized to more choices
Sigma.list <- mclapply(1:nrow(distribution.grid), function(i){
  
  d <- distribution.grid[i,]$d
  ij.mat <- t(combn(d,2))
  l.mat <- matrix(0,d,d)
  l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:nrow(ij.mat)
  
  Sh <- large.n*cov(t(replicate(N,{
    X <- generateData(n=large.n,
                      d=d,
                      tau=distribution.grid[i,]$tau,
                      dtau=distribution.grid[i,]$dtau,
                      dtau_type="none",
                      distribution=distribution.grid[i,]$distribution)
    
    cor.fk(X)[ij.mat]
  })))
  Sb <- averageSigma(Sh, l.mat, full=T)
})

distribution.grid$Sigma <- Sigma.list

Sigma.trans.list <- lapply(Sigma_list, function(Sh){
  eig <- eigen(Sh, symmetric = T)
  
  Sh2 <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
  Shi <- eig$vectors %*% diag(1/eig$values) %*% t(eig$vectors)
  Shi2 <- eig$vectors %*% diag(1/sqrt(eig$values)) %*% t(eig$vectors)
  
  return(list(Sigma2 = Sh2, SigmaI = Shi, SigmaI2 = Shi2))
})


distribution.grid$Sigma2 <- lapply(Sigma.trans.list,"[[","Sigma2")
distribution.grid$SigmaI <- lapply(Sigma.trans.list,"[[","SigmaI")
distribution.grid$SigmaI2 <- lapply(Sigma.trans.list,"[[","SigmaI2")


# Computation of rejection region -----------------------------------------

rejection.grid <- as.data.table(expand.grid(distribution_id = distribution.grid$distribution_id,
                                            stat_id = stat.grid$stat_id))

# Function for approximating quantiles of the null distribution
generateMCQuantile <- function(S.star, norm, M = 10000, probs = seq(0,1,.01)){
  p <- ncol(S.star)
  Z <- matrix(rnorm(M*p,0,1),M,p) %*% t(S.star)
  
  if(norm == "Euclidean") norm_Z <- apply(Z,1,crossprod)
  if(norm == "Supremum") norm_Z <- apply(abs(Z),1,max)
    
  return(quantile(norm_Z, probs = probs))
}

quantiles.list <- lapply(1:nrow(rejection.grid), function(i){
  
  d_id <- rejection.grid[i,]$distribution_id
  s_id <- rejection.grid[i,]$stat_id
  
  d <- distribution.grid[distribution_id == d_id,]$d
  Sigma <- distribution.grid[distribution_id == d_id,]$Sigma[[1]]
  Sigma2 <- distribution.grid[distribution_id == d_id,]$Sigma2[[1]]
  SigmaI <- distribution.grid[distribution_id == d_id,]$SigmaI[[1]]
  SigmaI2 <- distribution.grid[distribution_id == d_id,]$SigmaI2[[1]]

  S <- stat.grid[stat_id == s_id,]$S
  norm <- stat.grid[stat_id == s_id,]$norm

  # Define (pre-specified) hypothesis
  p <- choose(d,2)
  B <- matrix(1,p,1)
  
  # Compute S.star
  if(S == "I"){
    S <- Si <- Si2 <- diag(p)
  }else if(S == "Sh"){
    S <- Sigma
    Si <- SigmaI
    Si2 <- SigmaI2
  }

  G <- matrix(colSums(Si),p,p,byrow=T) / sum(Si)
  IG <- diag(p)-G
  S.star <- Si2 %*% IG %*% Sigma2
  
  # compute quantiles  
  generateMCQuantile(S.star = S.star, norm = norm)
})

rejection.grid$quantiles <- quantiles.list



# Computation of drift term -----------------------------------------------

zeta1.grid <- as.data.table(expand.grid(distribution_id = distribution.grid$distribution_id,
                                       stat_id = stat.grid$stat_id,
                                       dtau_type = dtau_type))

## BETA = 0 ?!?

zeta1.list <- lapply(1:nrow(zeta.grid), function(i){
  
  d_id <- zeta.grid[i,]$distribution_id
  s_id <- zeta.grid[i,]$stat_id
  dtau_type <- zeta.grid[i,]$dtau_type
  
  d <- distribution.grid[distribution_id == d_id,]$d
  Sigma <- distribution.grid[distribution_id == d_id,]$Sigma[[1]]
  Sigma2 <- distribution.grid[distribution_id == d_id,]$Sigma2[[1]]
  SigmaI <- distribution.grid[distribution_id == d_id,]$SigmaI[[1]]
  SigmaI2 <- distribution.grid[distribution_id == d_id,]$SigmaI2[[1]]
  
  S <- stat.grid[stat_id == s_id,]$S
  if(S == "I"){
    S <- Si <- Si2 <- diag(p)
  }else if(S == "Sh"){
    S <- Sigma
    Si <- SigmaI
    Si2 <- SigmaI2
  }
  norm <- stat.grid[stat_id == s_id,]$norm
  
  # Define (pre-specified) hypothesis
  p <- choose(d,2)
  B <- matrix(1,p,1)
  
  if(dtau_type == "single") dep_set <- 1
  if(dtau_type == "column") dep_set <- 1:(d-1)
  a <- sum(Si)
  b <- sum(Si[,dep_set])
  C <- matrix(0,p,p)
  C[dep_set,dep_set] <- 2*(a-b)
  C[dep_set,-dep_set] <- C[-dep_set,dep_set] <- a-2*b
  C[-dep_set,-dep_set] <- -2*b
  
  return(c(1/a^2 * C %*% Si %*% B))
})

zeta1.grid$zeta1 <- zeta1.list
zeta.grid <- as.data.table(merge.data.frame(zeta1.grid,data.frame(epsilon = epsilon.vec),all=T))
zeta.grid$zeta <- lapply(1:nrow(zeta.grid), function(k) zeta.grid$zeta1[[k]] * zeta.grid$epsilon[k])


# Test statistics generated under H1 --------------------------------------


## HERE!!*!*!*!*

# for each distribution, stat and departure; compute rejection rate for all quantiles

replicate(N,{
  X <- generateData(n=large.n,
                    d=d,
                    tau=distribution.grid[i,]$tau,
                    dtau=distribution.grid[i,]$dtau,
                    dtau_type="none",
                    distribution=distribution.grid[i,]$distribution)
  
  cor.fk(X)[ij.mat]
}))




# Computation of test statistics ------------------------------------------





# Compute rejection region (under H0) -------------------------------------





# Compute power curves ----------------------------------------------------





