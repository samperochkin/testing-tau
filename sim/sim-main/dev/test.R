
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFunLow.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFunLow(n = c(50),
          d = c(25),
          tau = c(0),
          dtau = c(0),
          distribution = c("normal"),
          num_sim = 500,
          filename = "dt_main_low_2_test",
          clus = rep(paste0("dms",1:6),4)
)
difftime(Sys.time(),tiime)
