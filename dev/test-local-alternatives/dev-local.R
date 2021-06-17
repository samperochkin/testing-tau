# packages
library(parallel)
library(mvtnorm)
library(data.table)
sapply(list.files("functionsLocal/", full.names = T), source, local = environment())

# parameters
n <- 100
d <- 3
tau <- .1
epsilon <- .2
distribution <- "normal"
num.sim <- 200
cores <- detectCores()-2

# create a parameter grid
sim.grid <- createGrid(n, d, tau, epsilon, distribution, num.sim)
sim.grid <- sim.grid[dtau_type == "single"]
sim.grid$M <- 2500

# for parallel stuff
res <- rbindlist(mclapply(sample(nrow(sim.grid)), function(r){

  # assign parameters -- just for clarity
  n <- sim.grid[r,]$n
  d <- sim.grid[r,]$d
  p <- choose(d,2)
  tau <- sim.grid[r,]$tau
  epsilon <- sim.grid[r,]$dtau
  dtau_type <- sim.grid[r,]$dtau_type
  distribution <- sim.grid[r,]$distribution
  M <- sim.grid[r,]$M
  
  X <- generateData(n,d,tau,epsilon/sqrt(n),dtau_type,distribution)
  
  # bias term
  epsilon.vec <- epsilon*tau/p * c(p-1,rep(-1,p-1))
  
  cbind(sim.grid[r,],performTestsAlternative(X,epsilon.vec,M=M))
}, mc.cores = cores))


dt <- res#[dtau_type == "single"]


dt[, epsilon := dtau] # watch out here
unique(dt$epsilon)

dt[,decision := pvalue < .05]
dt$Sh <- factor(dt$Sh, levels = c("ShP","ShJ","SbP","SbJ"))
dt$S <- factor(dt$S, levels = c("I"))

dt[,.N,.(tau,epsilon,n,d,norm,Sh,S,dtau_type)]

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("epsilon","tau","n","d","S","Sh","norm","dtau_type")]
dt2

library(ggplot2)
ggplot(dt, aes(x=pvalue,y=..density..)) +
  geom_histogram(breaks = seq(0,1,.1)) +
  facet_grid(norm~Sh+n)

library(ggplot2)
ggplot(dt, aes(sample = pvalue)) +
  geom_qq(distribution = stats::qunif,geom = "line") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  geom_abline(slope=1, intercept=0, col="red", linetype=2) +
  facet_grid(norm~Sh+n)


# Sh = I
xdt <- dcast(dt2[Sh %in% c("ShP","ShJ","SbP","SbJ")],
             formula = epsilon+tau+d ~ norm+Sh+S+n,
             value.var = "level")
xdt

xdt <- dcast(dt2[Sh %in% c("ShP","ShJ","SbP","SbJ")],
             formula = epsilon+tau+n+d+Sh ~ S+norm,
             value.var = "level")
xdt
print(xtable(xdt,digits=2),include.rownames = F)

xdt <- dcast(dt2[Sh %in% c("ShP","ShJ","SbP","SbJ") & S == "I"],
             formula = n+d+Sh ~ norm,
             value.var = "level")
xdt
print(xtable(xdt,digits=2),include.rownames = F)
