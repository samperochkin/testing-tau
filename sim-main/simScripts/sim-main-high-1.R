
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFunHigh2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2(n = c(50,100,150),
       d = c(50),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("normal"),
       num_sim = 2500,
       filename = "dt_main_high_1",
       clus = c(rep(paste0("dms",1:8),4),rep(paste0("dms",11:12),8))
)
difftime(Sys.time(),tiime)