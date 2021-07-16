library(data.table)
source("sim/sim-main/analysisScripts/tables/functions/constructDT.R")

fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
fii <- fii[!grepl("low_1", fii)]
fii <- fii[!grepl("low_2", fii)]
dt <- contructDT(fii)
fwrite(dt, "sim/sim-main/results/results-extra/dt_cc.csv")
