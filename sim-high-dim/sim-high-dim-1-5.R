
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

simFun(n = c(200),
       d = c(100),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("normal"),
       num_sim = 500,
       filename = "dt_high_dim_1_5",
       clus = c(rep(c("dms1","dms2","dms3","dms4","dms5"),3),rep(c("dms6","dms7"),3), rep("dms11",10))
)
