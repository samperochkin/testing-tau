

# Packages ----------------------------------------------------------------

library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(parallel)

# Setup -------------------------------------------------------------------


#*************** IMPORTANT TUNING PARAMETER
large.n <- 5000 # for asymptotic results..
epsilon <- 3


################
# distribution #
################
d <- c(5)
tau <- c(.7)
distribution <- c("normal")

distribution.grid <- as.data.table(expand.grid(distribution=distribution, tau=tau))
distribution.grid <- distribution.grid[!(distribution %in% c("clayton","gumbel") & tau == 0)]
distribution.grid[, sigma_id := 1:nrow(distribution.grid)]
distribution.grid <- as.data.table(merge.data.frame(distribution.grid, data.frame(d=d), all=T))
distribution.grid[, Sigma_id := 1:nrow(distribution.grid)]
distribution.grid[, distribution_id := 1:nrow(distribution.grid)]


#############
# departure #
#############
dtau_type <- c("single")

departure.grid <- as.data.table(expand.grid(dtau_type=dtau_type))
departure.grid[, departure_id := 1:nrow(departure.grid)]


###################
# test statistics #
###################
# S <- c("I", "Sh")
S <- c("I")
# norm = c("Euclidean", "Supremum")

# stat.grid <- as.data.table(expand.grid(S=S, norm=norm))
stat.grid <- as.data.table(expand.grid(S=S))
stat.grid[,stat_id := 1:nrow(stat.grid)]


##############
# FULL TABLE #
##############

full.grid <- Reduce(function(x,y) merge.data.frame(x, y, all=T),
                    list(distribution.grid,departure.grid,stat.grid))
full.grid <- as.data.table(full.grid)




# Estimation of Sigma (under H0) ------------------------------------------
# Here, I sample X from the null, and compute cov(tau.hat)
# I average over equal values to get a more precise estimation

source("sim/sim-main/functionsLow2/generateData.R")
source("sim/sim-main/functionsLow2/averageSigma.R")

N <- 10000
sigma.grid <- distribution.grid[,.(distribution = unique(distribution), tau = unique(tau)), .(sigma_id = sigma_id)]


  sigma.list <- mclapply(1:nrow(sigma.grid), function(i){
    
    d <- 10
    ij.mat <- t(combn(d,2))
    l.mat <- matrix(0,d,d)
    l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:nrow(ij.mat)
    
    # compute n*Sigma.hat
    Sh <- large.n * cov(t(replicate(N,{
      X <- generateData(n=large.n,
                        d=d,
                        tau=sigma.grid[i,]$tau,
                        dtau=0,
                        dtau_type="none",
                        distribution=sigma.grid[i,]$distribution)
      
      cor.fk(X)[ij.mat]
    })))
    
    # average of theoretically identical entries
    sb <- averageSigma(Sh, l.mat, full=F)
    
    return(sb)
  }, mc.cores = 6)


# Creating full Sigma matrices from 3d sigmas -----------------------------
  expandSigma <- function(sb, d){
    
    p <- choose(d,2)
    ij.mat <- t(combn(d,2))
    l.mat <- matrix(0,d,d)
    l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:nrow(ij.mat)
    
    Sb <- matrix(0,p,p)
    
    B <- Matrix::Matrix(0, nrow = p, ncol = d, sparse = T)
    for(i in 1:d){
      B[l.mat[i,-i],i] <- 1
    }
    BtB <- Matrix::tcrossprod(B)
    
    for(k in 0:2){
      Sb[Matrix::which(BtB == k)] <- sb[k+1]
    }
    
    eig <- eigen(Sb, symmetric = T)
    Sh2 <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
    Shi <- eig$vectors %*% diag(1/eig$values) %*% t(eig$vectors)
    Shi2 <- eig$vectors %*% diag(1/sqrt(eig$values)) %*% t(eig$vectors)
    
    return(list(Sigma = Sb, Sigma2 = Sh2, SigmaI = Shi, SigmaI2 = Shi2))
  }
  
  
  Sigma.list <- mclapply(1:nrow(distribution.grid), function(i){
    expandSigma(sigma.list[[distribution.grid[i]$sigma_id]],distribution.grid[i]$d)
  }, mc.cores = 6)
  

# Computation of S.star ---------------------------------------------------
# S.star is now S.dag in the paper... 
S.star.grid <- as.data.table(expand.grid(distribution_id = distribution.grid$distribution_id,
                                         S = unique(stat.grid$S)))
S.star.grid[, Sigma_id := distribution_id]
S.star.grid[, S_star_id := 1:nrow(S.star.grid)]
full.grid <- merge(full.grid, S.star.grid, by=c("distribution_id", "S", "Sigma_id"))

S.star.list <- mclapply(1:nrow(S.star.grid), function(i){
  
  # get relevant quantities
  d_id <- S.star.grid[i]$distribution_id
  
  d <- distribution.grid[distribution_id == d_id,]$d
  # Define (pre-specified) hypothesis
  p <- choose(d,2)
  B <- matrix(1,p,1)
  
  Sigma2 <- Sigma.list[[d_id]]$Sigma2
  
  S <- S.star.grid[i]$S
  if(S == "I"){
    S <- Si <- Si2 <- diag(p)
  }else if(S == "Sh"){
    S <- Sigma.list[[d_id]]$Sigma
    Si <- Sigma.list[[d_id]]$SigmaI
    Si2 <- Sigma.list[[d_id]]$SigmaI2
  }
  
  # Compute S.star
  G <- matrix(colSums(Si),p,p,byrow=T) / sum(Si)
  IG <- diag(p)-G
  Si2 %*% IG %*% Sigma2
}, mc.cores = 6)


# Computation of drift term -----------------------------------------------

zeta1.grid <- as.data.table(expand.grid(distribution_id=distribution.grid$distribution_id, dtau_type=dtau_type))
zeta1.grid[,zeta1_id := 1:nrow(zeta1.grid)]
full.grid <- merge(full.grid, zeta1.grid, by=c("distribution_id", "dtau_type"))

zeta1.grid <- merge(zeta1.grid, distribution.grid, by="distribution_id")
setorder(zeta1.grid,zeta1_id)

  source("sim/sim-main/powerCurves/functions/gFun.R")
  source("sim/sim-main/powerCurves/functions/scoreFunction.R")
  source("sim/sim-main/powerCurves/functions/computeZeta1.R")
  
  zeta1 <- computeZeta1(zeta1.grid[i,], N=1000) # it calls mclapply inside (for hacs)
  zeta <- zeta1 * epsilon
  
# NOTE: zeta1*epsilon IS a
# zeta1 should be equal to lim E{sqrt(n)*(tau.hat - tau)} under H1  with epsilon = 1

full.grid

source("sim/sim-main/functionsLow2/generateData.R")
full.grid
d <- full.grid$d
p <- choose(d,2)
a <- replicate(1000, {
    X <- generateData(n = large.n, d = full.grid$d,
                      tau = full.grid$tau, dtau = epsilon/sqrt(large.n),
                      dtau_type = full.grid$dtau_type,
                      distribution = full.grid$distribution)
    ij.mat <- t(combn(full.line$d,2))
    c((sqrt(large.n) * (cor.fk(X)[ij.mat] - full.grid$tau)))
  }) %>% rowMeans

round(rbind(zeta,a),4)
#




#########
#########
a1 <- sapply(1:nrow(full.grid), function(k){
  full.line <- full.grid[k]
  
  zeta1_id <- full.line$zeta1_id
  zeta1 <- zeta1.list[[zeta1_id]]
  
  d <- full.line$d
  p <- choose(d,2)
  
  S <- full.line$S
  d_id <- full.line$distribution_id
  if(S == "I"){
    Si <- Si2 <- diag(p)
  }else if(S == "Sh"){
    Si <- Sigma.list[[d_id]]$SigmaI
    Si2 <- Sigma.list[[d_id]]$SigmaI2
  }
  G <- matrix(colSums(Si),p,p,byrow=T) / sum(Si)
  IG <- diag(p)-G
  SIGz <- c(Si2 %*% IG %*% (zeta1*epsilon))
})    


source("sim/sim-main/functionsLow2/generateData.R")
a2 <- sapply(1:nrow(full.grid), function(k){
  full.line <- full.grid[k]
  
  zeta1_id <- full.line$zeta1_id
  zeta1 <- zeta1.list[[zeta1_id]]
  
  d <- full.line$d
  p <- choose(d,2)
  
  S <- full.line$S
  d_id <- full.line$distribution_id
  if(S == "I"){
    Si <- Si2 <- diag(p)
  }else if(S == "Sh"){
    Si <- Sigma.list[[d_id]]$SigmaI
    Si2 <- Sigma.list[[d_id]]$SigmaI2
  }
  G <- matrix(colSums(Si),p,p,byrow=T) / sum(Si)
  IG <- diag(p)-G

  a <- replicate(1000, {
    
    X <- generateData(n = large.n, d = full.line$d,
                      tau = full.line$tau, dtau = epsilon/sqrt(large.n),
                      dtau_type = full.line$dtau_type,
                      distribution = full.line$distribution)
    
    
  
    ij.mat <- t(combn(full.line$d,2))
    c((sqrt(large.n) * (cor.fk(X)[ij.mat] - full.line$tau)))
  }) %>% rowMeans
  
  c(Si2 %*% IG %*% a)
})    


a1
a2

