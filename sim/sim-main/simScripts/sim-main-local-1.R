
setwd("sim/sim-main")

# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLocal3.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLocal3(n = c(400),
          d = c(5,10),
          tau = c(.25),
          epsilon = c(5,10),
          distribution = c("normal"),
          num.sim = 1000,
          filename = "dt_main_local_8",
          cores = 8
)
difftime(Sys.time(),tiime)
