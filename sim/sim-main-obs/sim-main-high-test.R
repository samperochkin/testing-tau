
# packages ----------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)

# functions ---------------------------------------------------------------
source("functions/simFunHigh.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFun(n = c(200),
       d = c(50),
       tau = c(.3),
       dtau = c(0,.1),
       distribution = c("normal"),
       num_sim = 2,
       filename = "dt_main_high_test",
       # clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4))
)
difftime(Sys.time(),tiime)
