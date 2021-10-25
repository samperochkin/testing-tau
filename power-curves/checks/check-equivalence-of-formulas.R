

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


##
Sigma.list <- readRDS(paste0("sim/sim-main/powerCurves/Sigma_list_",2,".rds"))

zeta1.grid <- as.data.table(expand.grid(Sigma_id = distribution.grid$distribution_id,
                                        S = S,
                                        dtau_type = dtau_type))
zeta1.grid[, d := distribution.grid[Sigma_id]$d]
zeta1.grid[, zeta1_id := 1:nrow(zeta1.grid)]
full.grid <- merge(full.grid, zeta1.grid, by=c("Sigma_id", "S", "d", "dtau_type"))


z_id <- c(
zeta1.grid[S == "I" & dtau_type == "single" & d == 25]$zeta1_id[1],
zeta1.grid[S == "I" & dtau_type == "column" & d == 25]$zeta1_id[1],
zeta1.grid[S == "Sh" & dtau_type == "single" & d == 25]$zeta1_id[1],
zeta1.grid[S == "Sh" & dtau_type == "column" & d == 25]$zeta1_id[1]
)


###############
###############

lapply(z_id, function(i){
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
  if(dtau_type == "column") dep_set <- d:p
  a <- sum(Si)
  b <- sum(Si[,dep_set])
  C <- matrix(0,p,p)
  C[dep_set,dep_set] <- 2*(a-b)
  C[dep_set,-dep_set] <- C[-dep_set,dep_set] <- a-2*b
  C[-dep_set,-dep_set] <- -2*b
  
  ep <- rep(0,p)
  ep[dep_set] <- 1
  
  # identical(c(1/a^2 * Si2 %*% C %*% Si %*% B),
  #       c(Si2 %*% (-b/a + ep)))
  
  sum(abs(c(1/a^2 * Si2 %*% C %*% Si %*% B)-
            c(Si2 %*% (-b/a + ep))))
})
