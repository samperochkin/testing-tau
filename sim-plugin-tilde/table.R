setwd("sim-plugin-tilde")
library(data.table)
library(xtable)
dt <- fread("dt_plugin_tilde_2_1.csv")
dt <- dt[order(ID),]


# Size --------------------------------------------------------------------

dt.sub <- dt[dtau == 0 & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, method, n, d)]
dt.sub <- dcast(dt.sub, method ~ d, value.var = "size")
dt.sub


# power -------------------------------------------------------------------
## single

dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, method, n, d)]
dt.sub <- dcast(dt.sub, method ~ d, value.var = "power")
dt.sub

## column

dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, method, n, d)]
dt.sub <- dcast(dt.sub, method ~ d, value.var = "power")
dt.sub
