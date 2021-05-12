
setwd("sim/sim-main")

# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLocal3.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLocal3(n = c(400),
          d = c(5),
          tau = c(0,.25),
          epsilon = c(5),
          distribution = c("joe"),
          num.sim = 1000,
          filename = "dt_main_local_1",
          cores = 2
)
difftime(Sys.time(),tiime)
