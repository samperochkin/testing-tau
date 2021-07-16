
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
#setwd("sim-main")
source("simFuns/simFunLowBlock.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFunLowBlock(n = c(50,150,250),
          d = c(6,12,18),
          design = c("balanced","unbalanced"),
          dtau = c(0,.1,.2),
          distribution = c("t4"),
          num_sim = 1000,
          filename = "/store/samuel/testing-tau-extra/dt_main_low_block_1",
          cl = 12
)
difftime(Sys.time(),tiime)
