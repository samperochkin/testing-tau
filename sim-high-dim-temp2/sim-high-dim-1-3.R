
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
# should take one day

tiime <- Sys.time()
simFun(n = c(50,100,200),
       d = c(100),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("joe", "clayton", "normal", "cauchy"),
       num_sim = 1,
       filename = "dt_high_dim_1_3",
       clus = NULL#c(rep(c("dms2"),4), rep(c("dms3","dms4","dms5","dms6","dms7"),4), rep("dms11",8))
)
difftime(Sys.time(),tiime)