
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
       d = c(100),
       tau = c(.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("clayton", "gumbel"),
       num_sim = 1000,
       filename = "/store/samuel/testing-tau-extra/dt_main_boot_4_hac",
       clus = 6
)
difftime(Sys.time(),tiime)