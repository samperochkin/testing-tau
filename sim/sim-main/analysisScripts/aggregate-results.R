library(data.table)
source("sim/sim-main/analysisScripts/tables/functions/constructDT.R")

fii <- list.files("sim/sim-main/results/ulaval-server-results",full.names = T,pattern = "csv")
fii <- fii[!grepl("test", fii)]
fii <- fii[!grepl("boot_1", fii)] # there should not be any..
fii <- fii[!grepl("boot_2", fii)] # there should not be any..
fii <- fii[!grepl("low_3", fii)] # this is actually a bootstrap* version of low_1
fii <- fii[!grepl("low_4", fii)] # this is actually a bootstrap* version of low_2
fii <- fii[!grepl("block", fii)] # treated elsewhere

dt <- contructDT(fii, dis = "main") # normal not written in filename
print(object.size(dt), units = "MB")
fwrite(dt, "sim/sim-main/results/dt_exch_normal.csv")

dt <- rbind(dt,fread("sim/sim-main/results/results-extra/dt_exch_cc.csv"))
print(object.size(dt), units = "MB")
fwrite(dt, "sim/sim-main/results/dt_exch_full.csv")
