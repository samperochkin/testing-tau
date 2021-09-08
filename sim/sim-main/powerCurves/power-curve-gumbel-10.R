

# Packages ----------------------------------------------------------------

library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(parallel)

# Setup -------------------------------------------------------------------
mc_cores <- 8

#*************** IMPORTANT TUNING PARAMETER
family <- "gumbel"
d <- 10
run.id <- paste(family,d,sep="-")
large.n <- 10000 # for asymptotic results..
M <- 5000
epsilon <- seq(0,7.5,.1)


################
# distribution #
################
# d setup in preamble!
tau <- c(0,.25,.5,.75)
distribution <- c("gumbel")

distribution.grid <- as.data.table(expand.grid(distribution=distribution, tau=tau))
distribution.grid <- distribution.grid[!(distribution %in% c("clayton") & tau == 0)]
distribution.grid[, sigma_id := 1:nrow(distribution.grid)]
distribution.grid <- as.data.table(merge.data.frame(distribution.grid, data.frame(d=d), all=T))
distribution.grid[, Sigma_id := 1:nrow(distribution.grid)]
distribution.grid[, distribution_id := 1:nrow(distribution.grid)]


#############
# departure #
#############
dtau_type <- c("single", "column")

departure.grid <- as.data.table(expand.grid(dtau_type=dtau_type))
departure.grid[, departure_id := 1:nrow(departure.grid)]


###################
# test statistics #
###################
S <- c("I", "Sh")


stat.grid <- as.data.table(expand.grid(S=S))
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
sigma.grid <- distribution.grid[,.(distribution = unique(distribution), tau = unique(tau)), .(sigma_id = sigma_id)]

if(!file.exists(paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/sigma_list.rds"))){
  sigma.list <- mclapply(1:nrow(sigma.grid), function(i){
    
    d <- 10
    ij.mat <- t(combn(d,2))
    l.mat <- matrix(0,d,d)
    l.mat[ij.mat] <- l.mat[ij.mat[,2:1]] <- 1:nrow(ij.mat)
    
    Sh <- large.n*cov(t(replicate(N,{
      X <- generateData(n=large.n,
                        d=d,
                        tau=sigma.grid[i,]$tau,
                        dtau=0,
                        dtau_type="none",
                        distribution=sigma.grid[i,]$distribution)
      
      cor.fk(X)[ij.mat]
    })))
    sb <- averageSigma(Sh, l.mat, full=F)
    
    return(sb)
  }, mc.cores = mc_cores)
  saveRDS(sigma.list, paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/sigma_list.rds"))
}else{
  sigma.list <- readRDS(paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/sigma_list.rds"))
}

# Creating full Sigma matrices from 3d sigmas -----------------------------
if(!file.exists(paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/Sigma_list.rds"))){
  
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
  }, mc.cores = mc_cores)
  
  saveRDS(Sigma.list, paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/Sigma_list.rds"))
  
}else{
  Sigma.list <- readRDS(paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/Sigma_list.rds"))
}


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
}, mc.cores = mc_cores)



# Approximation of the null distribution quantiles  -----------------------
quantiles.grid <- as.data.table(expand.grid(S_star_id = S.star.grid$S_star_id))
quantiles.grid[,quantiles_id := 1:nrow(quantiles.grid)]
full.grid <- merge(full.grid, quantiles.grid, by=c("S_star_id"))

# Function for approximating quantiles of the null distribution
generateMCQuantile <- function(S.star, norm, Mq = M, probs = c(.9,.95,.975)){
  p <- ncol(S.star)
  Z <- matrix(rnorm(Mq*p,0,1),Mq,p) %*% t(S.star)
  
  list(Euclidean = quantile(apply(Z,1,crossprod), probs = probs),
       Supremum = quantile(apply(abs(Z),1,max), probs = probs))
}

quantiles.list <- mclapply(1:nrow(quantiles.grid), function(i){
  generateMCQuantile(S.star = S.star.list[[quantiles.grid[i]$S_star_id]], norm = quantiles.grid[i]$norm)
}, mc.cores = mc_cores)




# Computation of drift term -----------------------------------------------

zeta1.grid <- as.data.table(expand.grid(distribution_id=distribution.grid$distribution_id, dtau_type=dtau_type))
zeta1.grid[,zeta1_id := 1:nrow(zeta1.grid)]
full.grid <- merge(full.grid, zeta1.grid, by=c("distribution_id", "dtau_type"))

zeta1.grid <- merge(zeta1.grid, distribution.grid, by="distribution_id")
setorder(zeta1.grid,zeta1_id)

if(!file.exists(paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/zeta1_list.rds"))){
  
  source("sim/sim-main/powerCurves/functions/gFun.R")
  source("sim/sim-main/powerCurves/functions/scoreFunction.R")
  source("sim/sim-main/powerCurves/functions/computeZeta1.R")
  
  zeta1.list <- list()
  zeta1.list <- mclapply(1:nrow(zeta1.grid), function(i) computeZeta1(zeta1.grid[i,], N=10000), mc.cores = mc_cores)
  saveRDS(zeta1.list, paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/zeta1_list.rds"))
  
}else{
  zeta1.list <- readRDS(paste0("sim/sim-main/powerCurves/pre-computed/",run.id,"/zeta1_list.rds"))
}


# Compute power curves ----------------------------------------------------
mclapply(1:nrow(full.grid), function(i){
  
  # for each espilon, stat and quantile
  
  full.line <- full.grid[i]
  
  # normal component (centered)
  S_star_id <- full.line$S_star_id
  S.star <- S.star.list[[S_star_id]]
  p <- nrow(S.star)
  
  Z <- matrix(rnorm(M*p,0,1),M,p) %*% t(S.star)
  
  # drift term
  zeta1_id <- full.line$zeta1_id
  zeta1 <- zeta1.list[[zeta1_id]]
  
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
  SIGz <- c(Si2 %*% IG %*% zeta1)
  
  quantiles_id <- full.line$quantiles_id
  quantiles <- quantiles.list[[quantiles_id]]
  quant.grid <- data.table(quantile_id = 1:length(quantiles[[1]]), alpha = 1 - as.numeric(gsub("%","",names(quantiles[[1]])))/100)

  res.grid <- rbindlist(lapply(epsilon, function(eps){
    ZZ <- t(Z) + SIGz*eps
    euc <- colSums(ZZ*ZZ)
    sup <- apply(abs(ZZ),2,max)
    
    rbind(cbind(full.line, quant.grid, epsilon = eps, norm="Euclidean", power = sapply(quant.grid$quantile_id, function(k) mean(euc > quantiles$Euclidean[k]))),
          cbind(full.line, quant.grid, epsilon = eps, norm="Supremum", power = sapply(quant.grid$quantile_id, function(k) mean(sup > quantiles$Supremum[k]))))
  }))
  
  fwrite(res.grid, paste0("sim/sim-main/powerCurves/results/res_grid_",run.id,"_",i,".csv"))
  NULL
}, mc.cores = mc_cores)


# res.grid <- rbindlist(lapply(list.files("sim/sim-main/powerCurves/results",full.names = T), fread))
# res.grid[distribution == "gumbel"]
# 
# library(ggplot2)
# al <- .1
# ggplot(res.grid[round(alpha,3)==al & d == 5], aes(x=epsilon, y=power, col=norm, linetype=S)) +
#   theme_light() +
#   geom_line() +
#   geom_hline(yintercept=al, lty=2) +
#   ylim(c(0,1)) +
#   facet_grid(distribution~dtau_type+tau)
# 
# 
# rr <- res.grid[round(alpha,3)==al & dtau_type == "single" & d == 5 & norm == "Euclidean" & S == "I" & distribution == "normal" & tau == 0]
# rr[,]

# 
# ggplot(full.grid[norm == "Euclidean"], aes(x=epsilon, y=power, col=as.factor(round(alpha,3)))) + 
#   theme_light() +
#   geom_line() + 
#   ylim(c(0,1)) +
#   facet_wrap(~zeta1_id)
# 
# ggplot(full.grid[norm == "Euclidean"], aes(x=epsilon, y=power, col=as.factor(round(alpha,3)))) + 
#   theme_light() +
#   geom_line() + 
#   xlim(c(0,1)) +
#   ylim(c(0,.05)) +
#   facet_wrap(~zeta1_id)
# ggplot(full.grid[norm == "Supremum"], aes(x=epsilon, y=power, col=as.factor(round(alpha,3)))) + 
#   theme_light() +
#   geom_line() + 
#   xlim(c(0,.31)) +
#   ylim(c(0,.125)) +
#   facet_wrap(~zeta1_id)
# 
# 
# hist(full.grid[epsilon == 0, (alpha - power)/alpha])
# 
# 
# 
# al <- .1
# ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "normal"],
#        aes(x=epsilon, y=power, col=norm, linetype=S)) + 
#   theme_light() +
#   geom_line() + 
#   geom_hline(yintercept=al, lty=2) +
#   ylim(c(0,1)) +
#   facet_wrap(d~tau)
