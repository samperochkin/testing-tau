library(data.table)
source("sim/sim-main/analysisScripts/tables/functions/constructDT.R")

# fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
# dt <- contructDT(fii)
# fwrite(dt, "sim/sim-main/results/results-extra/dt_agg.csv")

fii <- list.files("sim/sim-main/results/ulaval-server-results",full.names = T,pattern = "csv")
fii <- fii[-c(6,11,12)]
dt <- contructDT(fii, dis = "main") # normal not written in filename
print(object.size(dt), units = "MB")

dt <- rbind(dt,fread("sim/sim-main/results/results-extra/dt_agg.csv"))
print(object.size(dt), units = "MB")
fwrite(dt, "sim/sim-main/results/dt_agg_full.csv")
