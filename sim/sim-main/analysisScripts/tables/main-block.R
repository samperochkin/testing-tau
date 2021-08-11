library(magrittr)
library(data.table)
library(xtable)


# data --------------------------------------------------------------------
dt <- fread("sim/sim-main/results/dt_block_full.csv")
dt$Sh <- factor(dt$Sh, levels = c("ShP", "ShJ", "SbP", "SbJ"))

dt_N <- dt[,unique(N),.(S,Sh,distribution)]
dt_N
dt_N <- dt[,.(min_N = min(N)),.(distribution)]

# table content -----------------------------------------------------------

#********
#******** To be worked out
#********
#********

gridH0 <- expand.grid(distribution = c("normal", "t4"),
                      dtau = c(0,.1)) %>% as.data.table
gridH0 <- merge(gridH0,dt_N, by="distribution")
gridH0$Sh <- "Sh"
gridH0$distribution <- factor(gridH0$distribution, levels = c("normal", "t4"))
setorder(gridH0,dtau,distribution)

gridH0.star <- copy(gridH0)
gridH0.star$Sh <- "Sb"  


# construction of tables --------------------------------------------------
sapply(list.files("sim/sim-main/analysisScripts/tables/functions",full.names = T), source)

grid <- rbind(gridH0,gridH0.star)

sapply(1:nrow(grid), function(k){
  tableBlockWrapper(dt, grid_line = grid[k])
})

