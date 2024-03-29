
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFunLow2.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow2(n = c(50,100,150,250),
       d = c(5,15),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("normal"),
       num_sim = 2500,
       filename = "dt_main_low_1",
       clus = rep(paste0("dms",1:7),4)
)
difftime(Sys.time(),tiime)