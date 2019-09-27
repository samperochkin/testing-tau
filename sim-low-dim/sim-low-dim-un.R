
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("functions/simFunUn.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &

simFunUn(n = c(50,100,200),
       d = c(5,10,20),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("normal"),
       num_sim = 2000,
       filename = "dt_low_dim_un",
       clus = c(rep(c("dms1","dms2"),3), rep(c("dms3","dms4","dms5","dms6","dms7"),4), rep("dms11",10))
       )
