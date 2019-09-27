
# packages ----------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)

# functions ---------------------------------------------------------------
source("functions/simFunMC.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &

simFun(n = c(100),
       d = c(350,500),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("normal"),
       num_sim = 2000,
       filename = "dt_high_dim_2",
       clus = c(rep(c("dms1","dms2"),3), rep(c("dms3","dms4","dms5","dms6","dms7"),3), rep("dms11",8))
       )
