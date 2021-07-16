
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------

source("simFuns/simFunLow2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2(n = c(250),
       d = c(5,15),
       tau = c(0,.3,.6),
       dtau = c(0),
       distribution = c("gumbel"),
       num_sim = 1000,
       filename = "/store/samuel/testing-tau-extra/dt_main_low_6_hac",
       clus = 4
)
difftime(Sys.time(),tiime)