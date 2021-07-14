library(data.table)
source("sim/sim-main/analysisScripts/tables/functions/constructDT.R")

fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
dt <- contructDT(fii)
print(object.size(dt), units = "MB")

fwrite(dt, "sim/sim-main/results/results-extra/dt_agg.csv")
