
# packages ----------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)

# functions ---------------------------------------------------------------
source("functions/simFun.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &

tiime <- Sys.time()
simFun(n = c(50),
       d = c(10),
       tau = c(.3),
       dtau = c(.1,.2),
       distribution = c("joe", "clayton", "normal", "cauchy"),
       num_sim = 2,
       filename = "dt_high_dim_1",
       clus = c(rep(c("dms2"),3), rep(c("dms3","dms4","dms5","dms6","dms7"),3), rep("dms11",8))
       )
difftime(Sys.time(),tiime)
