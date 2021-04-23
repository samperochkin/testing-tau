

# Packages ----------------------------------------------------------------

library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(parallel)

# Setup -------------------------------------------------------------------


#*************** IMPORTANT TUNING PARAMETER
large.n <- 10000 # for asymptotic results..
M <- 5000


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
epsilon <- seq(0,8,.1)
dtau_type <- c("single", "column")

departure.grid <- as.data.table(expand.grid(dtau_type=dtau_type, epsilon=epsilon))
departure.grid[, departure_id := 1:nrow(departure.grid)]


###################
# test statistics #
###################
# Sh <- c("ShJ", "SbJ")
Sh <- "ShJ"
S <- c("I", "Sh")
norm = c("Euclidean", "Supremum")

stat.grid <- as.data.table(expand.grid(S=S, norm=norm))
stat.grid[,stat_id := 1:nrow(stat.grid)]


##############
# FULL TABLE #
##############

full.grid <- Reduce(function(x,y) merge.data.frame(x, y, all=T),
                    list(distribution.grid,departure.grid,stat.grid))
full.grid <- as.data.table(full.grid)


# Estimation of Sigma (under H0) ------------------------------------------
source("sim/sim-main/functionsLow2/generateData.R")
source("sim/sim-main/functionsLow2/averageSigma.R")
N <- 10000

full.grid[, Sigma_id := distribution_id]
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
  
  eig <- eigen(Sb, symmetric = T)
  Sh2 <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
  Shi <- eig$vectors %*% diag(1/eig$values) %*% t(eig$vectors)
  Shi2 <- eig$vectors %*% diag(1/sqrt(eig$values)) %*% t(eig$vectors)
  
  return(list(Sigma = Sb, Sigma2 = Sh2, SigmaI = Shi, SigmaI2 = Shi2))
})





# Computation of S.star ---------------------------------------------------
S.star.grid <- as.data.table(expand.grid(distribution_id = distribution.grid$distribution_id,
                                         S = unique(stat.grid$S)))
S.star.grid[,Sigma_id := distribution_id] # for clarity
S.star.grid[,S_star_id := 1:nrow(S.star.grid)]
full.grid <- merge(full.grid, S.star.grid, by=c("distribution_id", "S", "Sigma_id"))

# S.star for computing rejection threshold AND power curves
S.star.list <- lapply(1:nrow(S.star.grid), function(i){
  
  # Define (pre-specified) hypothesis
  p <- choose(d,2)
  B <- matrix(1,p,1)
  
  # get relevant quantities
  d_id <- S.star.grid[i]$distribution_id
  Sigma_id <- S.star.grid[i]$Sigma_id
  
  d <- distribution.grid[distribution_id == d_id,]$d
  Sigma2 <- Sigma.list[[Sigma_id]]$Sigma2

  S <- S.star.grid[i]$S
  if(S == "I"){
    S <- Si <- Si2 <- diag(p)
  }else if(S == "Sh"){
    S <- Sigma.list[[Sigma_id]]$Sigma
    Si <- Sigma.list[[Sigma_id]]$SigmaI
    Si2 <- Sigma.list[[Sigma_id]]$SigmaI2
  }
  
  # Compute S.star
  G <- matrix(colSums(Si),p,p,byrow=T) / sum(Si)
  IG <- diag(p)-G
  Si2 %*% IG %*% Sigma2
})



# Approximation of the null distribution quantiles  -----------------------
quantiles.grid <- as.data.table(expand.grid(S_star_id = S.star.grid$S_star_id,
                                            norm = norm))
quantiles.grid[,quantiles_id := 1:nrow(quantiles.grid)]
full.grid <- merge(full.grid, quantiles.grid, by=c("S_star_id", "norm"))

# Function for approximating quantiles of the null distribution
generateMCQuantile <- function(S.star, norm, M = 10000, probs = c(.9,.95,.975,.99,.995,.999)){
  p <- ncol(S.star)
  Z <- matrix(rnorm(M*p,0,1),M,p) %*% t(S.star)
  
  if(norm == "Euclidean") norm_Z <- apply(Z,1,crossprod)
  if(norm == "Supremum") norm_Z <- apply(abs(Z),1,max)
    
  return(quantiles = quantile(norm_Z, probs = probs))
}

quantiles.list <- lapply(1:nrow(quantiles.grid), function(i){
  generateMCQuantile(S.star = S.star.list[[quantiles.grid[i]$S_star_id]], norm = quantiles.grid[i]$norm)
})


# Computation of drift term -----------------------------------------------

zeta1.grid <- as.data.table(expand.grid(Sigma_id = distribution.grid$distribution_id,
                                        S = S,
                                        dtau_type = dtau_type))
zeta1.grid[, d := distribution.grid[Sigma_id]$d]
zeta1.grid[, zeta1_id := 1:nrow(zeta1.grid)]

full.grid <- merge(full.grid, zeta1.grid, by=c("Sigma_id", "S", "d", "dtau_type"))


## BETA = 0 ?!?
zeta1.list <- lapply(1:nrow(zeta1.grid), function(i){
  
  d <- zeta1.grid[i]$d
  # Define (pre-specified) hypothesis
  p <- choose(d,2)
  B <- matrix(1,p,1)
  
  # Sigma
  Sigma_id <- zeta1.grid[i,]$Sigma_id
  Sigma <- Sigma.list[[Sigma_id]]$Sigma
  Sigma2 <- Sigma.list[[Sigma_id]]$Sigma2
  SigmaI <- Sigma.list[[Sigma_id]]$SigmaI
  SigmaI2 <- Sigma.list[[Sigma_id]]$SigmaI2
  
  # departure type
  dtau_type <- zeta1.grid[i,]$dtau_type
  
  # S
  S <- zeta1.grid[i,]$S
  if(S == "I"){
    S <- Si <- Si2 <- diag(p)
  }else if(S == "Sh"){
    S <- Sigma
    Si <- SigmaI
    Si2 <- SigmaI2
  }

  if(dtau_type == "single") dep_set <- 1
  if(dtau_type == "column") dep_set <- 1:(d-1)
  a <- sum(Si)
  b <- sum(Si[,dep_set])
  C <- matrix(0,p,p)
  C[dep_set,dep_set] <- 2*(a-b)
  C[dep_set,-dep_set] <- C[-dep_set,dep_set] <- a-2*b
  C[-dep_set,-dep_set] <- -2*b
  
  zeta1 <- c(1/a^2 * C %*% Si %*% B)
  if(dtau_type == "column") zeta1 <- -zeta1
  return(zeta1)
})



# Compute power curves ----------------------------------------------------

power.grid <- merge(S.star.grid,
                    zeta1.grid,
                    by = c("S", "Sigma_id"))

dev <- lapply(1:nrow(power.grid), function(i){
  print(i)
  
  power.line <- power.grid[i]
  
  S_star_id <- power.line$S_star_id
  S.star <- S.star.list[[S_star_id]]
  p <- nrow(S.star)
  
  zeta1_id <- power.line$zeta1_id
  zeta1 <- zeta1.list[[zeta1_id]]

  Z <- matrix(rnorm(M*p,0,1),M,p) %*% t(S.star)
  
  # new power grid
  new.quantiles.grid <- quantiles.grid[S_star_id == power.line$S_star_id,]
  quantiles <- quantiles.list[new.quantiles.grid$quantiles_id]  
  names(quantiles) <- new.quantiles.grid$norm
  
  new.power.grid <- power.line[c(1,1)]
  new.power.grid[,norm := new.quantiles.grid$norm]
  
  power1 <- lapply(epsilon, function(eps){
    if(new.power.grid[1]$norm == "Euclidean") Z_norm <- apply(t(Z) + eps*zeta1,2,crossprod)
    if(new.power.grid[1]$norm == "Supremum") Z_norm <- apply(abs(t(Z) + eps*zeta1),2,max)
    
    sapply(quantiles[[new.power.grid[1]$norm]], function(z) mean(Z_norm > z))
  })
  
  power2 <- lapply(epsilon, function(eps){
    if(new.power.grid[2]$norm == "Euclidean") Z_norm <- apply(t(Z) + eps*zeta1,2,crossprod)
    if(new.power.grid[2]$norm == "Supremum") Z_norm <- apply(abs(t(Z) + eps*zeta1),2,max)
    
    sapply(quantiles[[new.power.grid[2]$norm]], function(z) mean(Z_norm > z))
  })

  res.grid <- as.data.table(expand.grid(
              alpha = 1 - as.numeric(gsub("%","",names(quantiles[[1]])))/100,
              epsilon = epsilon,
              norm = new.power.grid$norm))
  res.grid[,power := unlist(c(power1,power2))]
  merge(res.grid, new.power.grid, by = "norm")
})



dev <- rbindlist(dev)
full.grid <- merge(full.grid, dev, by=intersect(names(full.grid),names(dev)))


library(ggplot2)
al <- .1
ggplot(full.grid[round(alpha,3)==al], aes(x=epsilon, y=power, col=norm)) + 
  theme_light() +
  geom_line() + 
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(~zeta1_id)

ggplot(full.grid[norm == "Euclidean"], aes(x=epsilon, y=power, col=as.factor(round(alpha,3)))) + 
  theme_light() +
  geom_line() + 
  ylim(c(0,1)) +
  facet_wrap(~zeta1_id)

ggplot(full.grid[norm == "Euclidean"], aes(x=epsilon, y=power, col=as.factor(round(alpha,3)))) + 
  theme_light() +
  geom_line() + 
  xlim(c(0,1)) +
  ylim(c(0,.05)) +
  facet_wrap(~zeta1_id)
ggplot(full.grid[norm == "Supremum"], aes(x=epsilon, y=power, col=as.factor(round(alpha,3)))) + 
  theme_light() +
  geom_line() + 
  xlim(c(0,.31)) +
  ylim(c(0,.125)) +
  facet_wrap(~zeta1_id)


hist(full.grid[epsilon == 0, (alpha - power)/alpha])
