
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLowBootLocal.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLowBootLocal(n = c(150,250),
       d = 25,
       tau = .3,
       delta = 5:8,
       distribution = c("normal", "t4"),
       num_sim = 2500,
       filename = "/store/samuel/testing-tau-extra/dt_main_local_boot_2",
       clus = 4
)
difftime(Sys.time(),tiime)