
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
setwd("sim-main")
source("dev/simFunLow-Q.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow(n = c(50),
          d = c(5),
          tau = c(.3),
          dtau = c(0,.1),
          distribution = c("normal"),
          num_sim = 2,
          filename = "dt_main_low_test",
          # clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4))
)
difftime(Sys.time(),tiime)