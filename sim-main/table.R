setwd("sim-main")
library(data.table)
library(xtable)
dt <- fread("dt_main_low_test.csv")
dt <- dt[order(ID),]


# Size --------------------------------------------------------------------

dt.sub <- dt[dtau == 0 & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, sigma + test ~ d, value.var = "size")
dt.sub


# power -------------------------------------------------------------------
## single

dt.sub <- dt[dtau == .15 & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, sigma + test ~ d, value.var = "power")
dt.sub

## column

dt.sub <- dt[dtau == .15 & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, sigma + test ~ d, value.var = "power")
dt.sub
