library(HAC)
library(mvtnorm)
library(pcaPP)
library(Matrix)

source("functions/generateData.R")
source("functions/testEquiRankSim.R")
M <- 500

tiime <- Sys.time()
pvals <- replicate(500, {
  X <- generateData(n = 100,
                    d = 5,
                    tau = .3,
                    dtau = 0,
                    #dtau_type = "column",
                    distribution = "joe")
  
  testEquiRankSim(X, M, M)
})
difftime(Sys.time(),tiime)

plot(pvals[1,])
plot(pvals[2,])

hist(pvals[1,])
hist(pvals[2,])

quantile(pvals[2,], seq(0,1,.05))
