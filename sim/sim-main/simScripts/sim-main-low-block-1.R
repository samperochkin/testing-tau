
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
#setwd("sim-main")
#source("simFuns/simFunLowBlock.R")
source("simFunLowBlock.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFunLowBlock(n = c(50,150,250),
          d = c(6,12,18),
          design = c("balanced","unbalanced"),
          dtau = c(0,.1,.2),
          distribution = c("normal"),
          num_sim = 2500,
          filename = "dt_main_low_block_1",
          cl = c(rep(c("dms11","dms12"),8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7","dms8"),4))
)
difftime(Sys.time(),tiime)
