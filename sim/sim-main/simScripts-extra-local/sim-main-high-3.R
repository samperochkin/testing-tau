
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunHigh2Local.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2Local(
       n = c(50,150,250),
       d = c(25),
       tau = .3,
       delta = seq(1,9,2),
       distribution = c("normal"),
       num_sim = 2500,
       filename = "/store/samuel/testing-tau-extra/dt_main_local_high_3",
       clus = 4
)
difftime(Sys.time(),tiime)