library(data.table)
library(xtable)


# data --------------------------------------------------------------------
dt <- fread("sim/sim-main/results/dt_exch.csv")
dt$Sh <- factor(dt$Sh, levels = c("ShP", "ShJ", "SbP", "SbJ"))

dt_N <- dt[,unique(N),.(S,Sh,distribution)]
dt_N
dt_N <- dt[,.(min_N = min(N)),.(distribution)]

# table content -----------------------------------------------------------
gridH0 <- rbind(
  expand.grid(distribution = c("normal", "t4", "clayton", "gumbel"),
              dtau_type = c("none"), dtau = 0),
  expand.grid(distribution = c("normal", "t4", "clayton", "gumbel"),
              dtau_type = c("departure"), dtau = c(.1,.2))
) %>% as.data.table
gridH0 <- merge(gridH0,dt_N, by="distribution")
gridH0$Sh <- "Sh"
gridH0$distribution <- factor(gridH0$distribution, levels = c("normal", "t4", "gumbel", "clayton"))
setorder(gridH0,dtau,distribution)

gridH0.star <- rbind(
  expand.grid(distribution = c("normal", "t4", "clayton", "gumbel"),
              dtau_type = c("none"), dtau = 0),
  expand.grid(distribution = c("normal", "t4", "clayton", "gumbel"),
              dtau_type = c("single", "column"), dtau = c(.1,.2))
) %>% as.data.table
gridH0.star <- merge(gridH0.star, dt_N, by="distribution")
gridH0.star$Sh <- "Sb"
gridH0.star$distribution <- factor(gridH0.star$distribution, levels = c("normal", "t4", "gumbel", "clayton"))
gridH0.star$dtau_type <- factor(gridH0.star$dtau_type, levels = c("none", "single", "column"))
setorder(gridH0.star,dtau_type,dtau,distribution)
gridH0.star

# construction of tables --------------------------------------------------
sapply(list.files("sim/sim-main/analysisScripts/tables/functions",full.names = T), source)

sapply(1:nrow(gridH0), function(k){
  tableWrapper(dt, grid_line = gridH0[k])
})


tableWrapper(dt, grid_line = gridH0[2])
tableWrapper(dt, grid_line = gridH0[5])
tableWrapper(dt, grid_line = gridH0[9])
tableWrapper(dt, grid_line = gridH0.star[17])
