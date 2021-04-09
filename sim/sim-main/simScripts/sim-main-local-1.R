
setwd("sim/sim-main")

# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLocal3.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLocal3(n = c(100,250,400),
          d = c(5,10),
          tau = c(.25,.5),
          epsilon = c(.5,1,1.5),
          distribution = c("normal"),
          num.sim = 1000,
          filename = "dt_main_local_6",
          cores = 8
)
difftime(Sys.time(),tiime)
