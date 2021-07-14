
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLow2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2(n = c(50,100,150,250),
       d = c(5,15,25),
       tau = c(0),
       dtau = c(0,.1,.2),
       distribution = c("gumbel"),
       num_sim = 1000,
       filename = "/store/samuel/testing-tau-extra/dt_main_low_gumbel0",
       clus = 4
)
difftime(Sys.time(),tiime)