

# Packages ----------------------------------------------------------------

library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(parallel)

# Setup -------------------------------------------------------------------


large.n <- 50000 # for asymptotic results..
M <- 10000


################
# distribution #
################
d <- c(5,25,50)
tau <- c(0,.25,.5,.75)
distribution <- c("normal","cauchy","joe")

distribution.grid <- as.data.table(expand.grid(distribution=distribution, tau=tau))
distribution.grid[, sigma_id := 1:nrow(distribution.grid)]
distribution.grid <- as.data.table(merge.data.frame(distribution.grid, data.frame(d=d), all=T))
distribution.grid[, Sigma_id := 1:nrow(distribution.grid)]
distribution.grid[, distribution_id := 1:nrow(distribution.grid)]


#############
# departure #
#############
epsilon <- seq(0,10,.1)
dtau_type <- c("single", "column")

departure.grid <- as.data.table(expand.grid(dtau_type=dtau_type, epsilon=epsilon))
departure.grid[, departure_id := 1:nrow(departure.grid)]


###################
# test statistics #
###################
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

N <- 50000
sigma.grid <- distribution.grid[,.(distribution = unique(distribution), tau = unique(tau)), .(sigma_id = sigma_id)]
sigma.list <- readRDS(paste0("sim/sim-main/powerCurves/sigma_list_",1,".rds"))
Sigma.list <- readRDS(paste0("sim/sim-main/powerCurves/Sigma_list_",1,".rds"))


# Computation of S.star ---------------------------------------------------
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


# Approximation of the null distribution quantiles  -----------------------
quantiles.grid <- as.data.table(expand.grid(S_star_id = S.star.grid$S_star_id,
                                            norm = norm))
quantiles.grid[,quantiles_id := 1:nrow(quantiles.grid)]
full.grid <- merge(full.grid, quantiles.grid, by=c("S_star_id", "norm"))

# Function for approximating quantiles of the null distribution
generateMCQuantile <- function(S.star, norm, Mq = M, probs = c(.9,.95,.975,.99,.995,.999)){
  p <- ncol(S.star)
  Z <- matrix(rnorm(Mq*p,0,1),Mq,p) %*% t(S.star)
  
  if(norm == "Euclidean") norm_Z <- apply(Z,1,crossprod)
  if(norm == "Supremum") norm_Z <- apply(abs(Z),1,max)
  
  return(quantiles = quantile(norm_Z, probs = probs))
}

quantiles.list <- mclapply(1:nrow(quantiles.grid), function(i){
  generateMCQuantile(S.star = S.star.list[[quantiles.grid[i]$S_star_id]], norm = quantiles.grid[i]$norm)
}, mc.cores = 6)


# Computation of drift term -----------------------------------------------

zeta1.grid <- as.data.table(expand.grid(Sigma_id = distribution.grid$distribution_id,
                                        S = S,
                                        dtau_type = dtau_type))
zeta1.grid[, d := distribution.grid[Sigma_id]$d]
zeta1.grid[, zeta1_id := 1:nrow(zeta1.grid)]

full.grid <- merge(full.grid, zeta1.grid, by=c("Sigma_id", "S", "d", "dtau_type"))

zeta1.list <- mclapply(1:nrow(zeta1.grid), function(i){
  
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
  
  zeta1 <- c(1/a^2 * Si2 %*% C %*% Si %*% B)
  if(dtau_type == "column") zeta1 <- -zeta1
  return(zeta1)
}, mc.cores = 6)

# not needed anymore
rm(Sigma.list)

# Compute power curves ----------------------------------------------------

power.grid <- merge(S.star.grid,
                    zeta1.grid,
                    by = c("S", "Sigma_id"))

powers <- mclapply(1:nrow(power.grid), function(i){

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
}, mc.cores = 6)


powers <- rbindlist(powers)
full.grid <- merge(full.grid, powers, by=intersect(names(full.grid),names(powers)))

fwrite(full.grid, paste0("sim/sim-main/powerCurves/full_grid_",1,"-test.csv"))