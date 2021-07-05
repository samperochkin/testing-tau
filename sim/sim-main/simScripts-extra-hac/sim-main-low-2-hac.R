
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLow2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2(n = c(50,100,150),
          d = c(25),
          tau = c(.3,.6),
          dtau = c(0,.1,.2),
          distribution = c("clayton", "gumbel"),
          num_sim = 1000,
          filename = "/store/samuel/testing-tau-extra/dt_main_low_2_hac",
          clus = 3
)
difftime(Sys.time(),tiime)
