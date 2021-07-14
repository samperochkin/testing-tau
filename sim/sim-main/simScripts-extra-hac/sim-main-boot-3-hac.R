
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------

#**************#
## ** BOOT ** ##
#**************#
source("simFuns/simFunLowBoot.R")
#**************#
## ** BOOT ** ##
#**************#

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLowBoot(n = c(50,100,150),
       d = c(50),
       tau = c(.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("clayton", "gumbel"),
       num_sim = 1000,
       filename = "/store/samuel/testing-tau-extra/dt_main_boot_3_hac",
       clus = 4
)
difftime(Sys.time(),tiime)