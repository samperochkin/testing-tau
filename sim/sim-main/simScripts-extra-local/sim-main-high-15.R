
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunHigh2Local.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2Local(
       n = c(3000),
       d = c(25),
       tau = .3,
       subset_dtau_type = c("single", "column"),
       delta = 2,
       distribution = c("normal"),
       num_sim = 2000,
       filename = "/store/samuel/testing-tau-extra/dt_main_local_high_15",
       clus = 8
)
difftime(Sys.time(),tiime)