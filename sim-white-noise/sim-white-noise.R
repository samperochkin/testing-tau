
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
       d = c(25),
       tau = c(0,.5),
       distribution = c("normal"),
       num_sim = 2000,
       filename = "dt_white_noise",
       clus = c(rep("dms1","dms2",3), rep(c("dms3","dms4","dms5","dms6","dms7"),4), rep("dms11",10))
)
