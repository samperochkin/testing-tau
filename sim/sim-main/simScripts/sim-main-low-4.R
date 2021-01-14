
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------

#**************#
## ** BOOT ** ##
#**************#
source("simFunLowBoot.R")
#**************#
## ** BOOT ** ##
#**************#

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLowBoot(n = c(50,100,150),
       d = c(25),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("normal"),
       num_sim = 2500,
       filename = "dt_main_low_4",
       # clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5"),4))
       clus = c(rep(paste0("dms",c(8:10,12)),4),rep("dms11",8))
)
difftime(Sys.time(),tiime)