
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunHigh2Local.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2Local(
       n = c(1500),
       d = c(15),
       tau = .3,
       delta = 2,
       distribution = c("normal"),
       num_sim = 1000,
       filename = "/store/samuel/testing-tau-extra/dt_main_local_high_8",
       clus = 12
)
difftime(Sys.time(),tiime)