
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunHigh2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2(n = c(50,100,150),
       d = c(50),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("t4"),
       num_sim = 1000,
       filename = "/store/samuel/testing-tau-extra/dt_main_high_1_t4",
       clus = 6
)
difftime(Sys.time(),tiime)