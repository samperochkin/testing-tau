
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFunHigh2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunHigh2(n = c(50,100),
       d = c(100),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("cauchy"),
       num_sim = 1000,
       filename = "dt_main_high_3_cauchy",
       clus = c(rep(paste0("dms",1:2),4),rep(paste0("dms",3:10),3),rep(paste0("dms",11:12),8))
)
difftime(Sys.time(),tiime)