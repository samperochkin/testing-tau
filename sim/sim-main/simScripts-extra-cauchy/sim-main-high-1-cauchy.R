
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
       distribution = c("cauchy"),
       num_sim = 1000,
       filename = "dt_main_high_1_cauchy",
       clus = c(rep(paste0("dms",1:10),4),rep(paste0("dms",11:12),8))
)
difftime(Sys.time(),tiime)