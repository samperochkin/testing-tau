
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunHigh2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2(n = c(50,100,150),
       d = c(15),
       tau = c(0,.3,.6),
       dtau = c(0,.1),
       distribution = c("normal"),
       num_sim = 2500,
       filename = "results/extra-for-presentation/dt_main_high_5_normal",
       clus = 6
)
difftime(Sys.time(),tiime)