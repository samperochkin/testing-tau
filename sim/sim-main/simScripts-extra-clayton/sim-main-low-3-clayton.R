
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
       d = c(5,15),
       tau = c(0,.3,.6),
       dtau = c(0,.1,.2),
       distribution = c("clayton"),
       num_sim = 1000,
       filename = "dt_main_low_3_clayton",
       # clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5"),4))
       clus = c(rep("dms11",8),rep("dms10",4),rep("dms12",12))
)
difftime(Sys.time(),tiime)