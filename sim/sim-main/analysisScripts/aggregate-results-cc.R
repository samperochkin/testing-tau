library(data.table)
source("sim/sim-main/analysisScripts/tables/functions/constructDT.R")

fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
fii <- fii[!grepl("boot_1", fii)]
fii <- fii[!grepl("boot_2", fii)]
fii <- fii[!grepl("low_3", fii)]
fii <- fii[!grepl("low_4", fii)]
dt <- contructDT(fii)
fwrite(dt, "sim/sim-main/results/results-extra/dt_exch_cc.csv")
