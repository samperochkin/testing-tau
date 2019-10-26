setwd("sim-high-dim")
library(data.table)
library(xtable)

dt <- rbindlist(list(fread("dt_high_dim_3_1.csv"),fread("dt_high_dim_3_2.csv")))
dt <- dt[order(ID),]


# Size --------------------------------------------------------------------
## Normal only

dt.sub <- dt[,.(size = mean(pval < .05)), .(distribution, method, n, d, dtau, dtau_type)]
dt.sub$dtau_type <- factor(dt.sub$dtau_type, levels = unique(dt.sub$dtau_type))
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, d + n ~ method + dtau_type + dtau, value.var = "size")
dt.sub

print(xtable(dt.sub, digits = 3), include.rownames=FALSE)



dt.sub <- dt[,.(size = mean(pval < .05)), .(distribution, method, n, d, dtau, dtau_type)]
dt.sub$dtau_type <- factor(dt.sub$dtau_type, levels = unique(dt.sub$dtau_type))
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, n ~ dtau_type + dtau, value.var = "size")
dt.sub

print(xtable(dt.sub, digits = 3), include.rownames=FALSE)





# dt.sub <- dt[,.(size = mean(pval < .05)), .(distribution, method, n, d, tau,  dtau, dtau_type)]
# dt.sub$dtau_type <- factor(dt.sub$dtau_type, levels = unique(dt.sub$dtau_type))
# 
# dt.sub <- dcast(dt.sub[dtau == 0], distribution + method ~ n + tau , value.var = "size")
# dt.sub
# 
# print(xtable(dt.sub, digits = 3), include.rownames=FALSE)
