
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunHigh2Local.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2Local(
       n = c(500),
       d = c(25),
       tau = .3,
       delta = 1:3,
       distribution = c("normal", "t4"),
       num_sim = 2500,
       filename = "/store/samuel/testing-tau-extra/dt_main_local_high_5",
       clus = 8
)
difftime(Sys.time(),tiime)