library(HAC)
library(mvtnorm)
library(pcaPP)
library(Matrix)

source("functions/generateData.R")
source("functions/testEquiRankBoot.R")
M <- 500

tiime <- Sys.time()
pvals <- replicate(500, {
  X <- generateData(n = 125,
                    d = 25,
                    tau = .3,
                    dtau = 0,
                    #dtau_type = "column",
                    distribution = "joe")
  
  testEquiRankBoot(X, M)
})
difftime(Sys.time(),tiime)

plot(pvals)
hist(pvals)
