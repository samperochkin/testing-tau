
# packages ----------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)

# functions ---------------------------------------------------------------
source("functions/simFunCorrection.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFun(n = c(50,150),
       d = c(5,10),
       tau = c(0,.3),
       dtau = c(0,.1),
       distribution = c("normal"),
       num_sim = 2000,
       filename = "dt_correction_1",
       clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4))
)
difftime(Sys.time(),tiime)
