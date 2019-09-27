
# packages ----------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)

# functions ---------------------------------------------------------------
source("functions/simFun2.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &

simFun(n = c(150),
       d = c(5,10,20),
       tau = c(.3),
       dtau = c(0,.1),
       distribution = c("normal"),
       num_sim = 2000,
       filename = "dt_plugin_tilde_2",
       clus = c(rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4), rep("dms11",8))
       )
