
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLow2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2(n = c(50,100,150),
       d = c(5,15),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("t4"),
       num_sim = 2,
       filename = "/store/samuel/testing-tau-extra/dt_main_low_1_t4",
       clus = 2)
difftime(Sys.time(),tiime)