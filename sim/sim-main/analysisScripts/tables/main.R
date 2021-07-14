library(data.table)
library(xtable)

sapply(list.files("sim/sim-main/analysisScripts/tables/functions", full.names = T), source)

fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
dt <- contructDT(fii)

expand.grid(generating_function = c(tableSize),
            distribution = c("normal", "t4", "clayton", "gumbel"))


R <- cbind(0,0,0,R)
tableSize(R=R, M=1000, "Clayton")

