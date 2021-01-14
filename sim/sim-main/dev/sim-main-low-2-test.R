
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
setwd("sim-main")
source("simFuns/simFunLow2.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFunLow2(n = c(150),
          d = c(25),
          tau = c(.6),
          dtau = c(0),
          distribution = c("normal"),
          num_sim = 3,
          filename = "dt_main_low_2_test",
          # clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4))
)
difftime(Sys.time(),tiime)
