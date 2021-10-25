
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
setwd("sim/sim-main/")
source("simFuns/simFunHigh2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2(n = c(500),
       d = c(5,15,25),
       tau = c(.3),
       dtau = c(.08),
       distribution = c("normal", "t4", "gumbel", "clayton"),
       num_sim = 5000,
       filename = "/store/samuel/testing-tau-extra/dt_main_high_500_1",
       clus = 12
)
difftime(Sys.time(),tiime)
