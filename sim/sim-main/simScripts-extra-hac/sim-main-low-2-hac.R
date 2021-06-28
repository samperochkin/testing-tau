
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFunLow2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2(n = c(50,100,150),
          d = c(25),
          tau = c(.3,.6),
          dtau = c(0,.1,.2),
          distribution = c("clayton", "gumbel"),
          num_sim = 1000,
          filename = "dt_main_low_2_clayton",
          clus = c(rep(paste0("dms",8:9),4),rep(paste0("dms",11:12),8))
)
difftime(Sys.time(),tiime)
