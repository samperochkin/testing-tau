
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
# should take much less than a day

simFun(n = c(50,100,200),
       d = c(10,25),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("joe", "clayton", "normal", "cauchy"),
       num_sim = 2000,
       filename = "dt_high_dim_1_1",
       clus = c(rep(c("dms2"),3), rep(c("dms3","dms4","dms5","dms6","dms7"),3), rep("dms11",8))
)
