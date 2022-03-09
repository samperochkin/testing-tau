
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLow2Local.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2Local(
  n = c(500),
  d = c(5),
  tau = .3,
  delta = 0:3,
  distribution = c("normal", "t4"),
  num_sim = 2500,
  filename = "/store/samuel/testing-tau-extra/dt_main_local_low_5",
  clus = 4
)
difftime(Sys.time(),tiime)