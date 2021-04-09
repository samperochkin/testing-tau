
setwd("sim/sim-main")

# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLocal3.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLocal3(n = c(100,300),
          d = c(4),
          tau = c(.25),
          epsilon = c(.5,1.5),
          distribution = c("normal"),
          num.sim = 1000,
          filename = "dt_main_local_5",
          cores = 8
)
difftime(Sys.time(),tiime)
