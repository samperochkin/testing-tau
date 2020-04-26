
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
          d = c(5),
          tau = c(.3),
          dtau = c(0,.1),
          distribution = c("normal"),
          num_sim = 2,
          filename = "dt_main_low_test",
          # clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4))
)
difftime(Sys.time(),tiime)

tiime <- Sys.time()
simFunLow(n = c(50,100,150),
       d = c(5,15,25),
       tau = c(0,.3,.6),
       dtau = c(0,.1),
       distribution = c("normal"),
       num_sim = 2500,
       filename = "dt_main_low",
       # clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4))
)
difftime(Sys.time(),tiime)
