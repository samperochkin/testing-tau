setwd("sim-chen-exch")
library(data.table)
library(xtable)

dt <- rbindlist(list(fread("dt_chen_exch_4.csv"),
                     fread("dt_chen_exch_5.csv"),
                     fread("dt_chen_exch_6.csv")))
dt <- dt[order(ID),]

# Size --------------------------------------------------------------------
## Normal only

dt.sub <- dt[dtau == 0 & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, d + method ~ tau + n, value.var = "size")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,3), .SDcols=nn[-c(1:2)]]

print(xtable(dt.sub, digits = 3), include.rownames=FALSE)



# power -------------------------------------------------------------------
## single
### normal only

dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "power")
dt.sub <- dcast(dt.sub, d + method ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,3), .SDcols=nn[-c(1:2)]]

print(xtable(dt.sub, digits = 3), include.rownames=FALSE)



## column
### normal only

dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "power")
dt.sub <- dcast(dt.sub, d + method ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,3), .SDcols=nn[-c(1:2)]]

print(xtable(dt.sub, digits = 3), include.rownames=FALSE)

