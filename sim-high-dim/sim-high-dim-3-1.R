
# packages ----------------------------------------------------------------
library(data.table)
library(mvtnorm)
library(HAC)
library(pcaPP)
library(Matrix)

# functions ---------------------------------------------------------------
source("functions/simFunJack.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

simFun(n = c(50,100,200),
       d = c(500),
       tau = c(.3),
       dtau = c(0,.1,.2),
       distribution = c("normal"),
       num_sim = 2000,
       filename = "dt_high_dim_3_1",
       clus = c(rep(c("dms1","dms2"),2), rep(c("dms3","dms4","dms5","dms10"),1), rep(c("dms6","dms7"),3), rep("dms11",12))
)
