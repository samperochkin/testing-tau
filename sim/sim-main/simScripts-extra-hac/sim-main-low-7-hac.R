
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------

source("simFuns/simFunLow2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2(n = c(250),
       d = c(25),
       tau = c(0,.3,.6),
       dtau = c(0),
       distribution = c("gumbel","clayton"),
       num_sim = 1000,
       filename = "/store/samuel/testing-tau-extra/dt_main_low_7_hac",
       clus = 10
)
difftime(Sys.time(),tiime)