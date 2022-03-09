
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLow2Local.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2Local(
  n = c(150,250),
  d = c(5),
  tau = .3,
  delta = 3:5,
  distribution = c("normal"),
  num_sim = 2500,
  filename = "/store/samuel/testing-tau-extra/dt_main_local_low_3",
  clus = 1
)
difftime(Sys.time(),tiime)