
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFunLow2Local.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2Local(
       n = c(50,150,250),
       d = c(25),
       tau = .3,
       delta = 0:10,
       distribution = c("t4"),
       num_sim = 2500,
       filename = "/store/samuel/testing-tau-extra/dt_main_local_low_4",
       clus = 4
)
difftime(Sys.time(),tiime)