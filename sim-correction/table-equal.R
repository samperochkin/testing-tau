setwd("sim-correction")
library(data.table)
library(xtable)
dt <- fread("dt_equal_1.csv")
dt <- dt[order(ID),]

dt[,isNA := is.na(pval)]
dt.sub <- dt[dtau == 0 & test == "sup" & force_constraints == F, .(meanNA = round(mean(isNA),2)), .(tau, n, d, sigma)]
dt.sub <- dcast(dt.sub, tau + d + n ~ sigma, value.var = "meanNA")
dt.sub

# Size --------------------------------------------------------------------

dt.sub <- dt[dtau == 0 & distribution == "normal" & sigma == "plugin", .(size = mean(pval < .05)), .(distribution, tau, test, n, d, correction, force_constraints)]
dt.sub <- dcast(dt.sub, d + test ~ n + tau + force_constraints, value.var = "size")
dt.sub

dt.sub <- dt[dtau == 0 & distribution == "normal" & sigma == "plugin", .(size = round(mean(pval < .05, na.rm=T),3)), .(distribution, tau, test, n, d, correction, force_constraints)]
dt.sub <- dcast(dt.sub, d + test ~ n + tau + force_constraints, value.var = "size")
dt.sub


# # power -------------------------------------------------------------------
# ## single
# 
# dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "single" & sigma == "plugin", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
# dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")
# dt.sub
# 
# ## column
# 
# dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
# dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")
# dt.sub
