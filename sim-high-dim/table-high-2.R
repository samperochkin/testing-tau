setwd("sim-high-dim")
library(data.table)
library(xtable)

tabs <- grep("dt_high_dim_2", list.files(), value = T)
dt <- rbindlist(lapply(tabs, fread))
dt <- dt[order(ID),]
rm(tabs)


# Size --------------------------------------------------------------------
## Normal only

dt.sub <- dt[,.(size = mean(pval < .05)), .(distribution, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, d + n ~ method, value.var = "size")
dt.sub

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)

