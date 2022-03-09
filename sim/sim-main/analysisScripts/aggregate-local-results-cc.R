library(data.table)
source("sim/sim-main/analysisScripts/tables/functions/constructDTLocal.R")

fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
fii <- fii[grepl("local", fii)]
dt <- contructDTLocal(fii)
fwrite(dt, "sim/sim-main/results/results-extra/dt_local_cc.csv")

# library(ggplot2)
# dt <- dt[tau+dtau < 1]
# dt_none <- dt[dtau_type == "none"]
# dt_single <- dt_column <- dt_none
# dt_single$dtau_type <- "single"
# dt_column$dtau_type <- "column"
# dt <- rbind(dt[dtau_type != "none"], dt_single, dt_column)
# 
# ggplot(dt[Sh == "SbJ" & n %in% c(150,250)], aes(x = delta, y = rejection_rate, col=as.factor(n), shape = S, linetype = norm)) +
#   geom_line() +
#   geom_point() +
#   facet_grid(d ~ dtau_type + distribution)
# 
# ggplot(dt[Sh == "SbJ" & n == 250], aes(x = delta, y = rejection_rate, shape = S, linetype = norm)) +
#   theme_light() +
#   coord_cartesian(xlim = c(0,7.5)) +
#   geom_line() +
#   geom_point() +
#   facet_grid(d ~ dtau_type + distribution)
# 
# ggplot(dt[Sh == "ShJ" & n == 250], aes(x = delta, y = rejection_rate, shape = S, linetype = norm)) +
#   theme_light() +
#   coord_cartesian(xlim = c(0,7.5)) +
#   geom_line() +
#   geom_point() +
#   facet_grid(d ~ dtau_type + distribution)
# 
# ggplot(dt[d == 5 & n == 50], aes(x = delta, y = rejection_rate, shape = S, linetype = norm, col = Sh)) +
#   theme_light() +
#   geom_line() +
#   geom_point() +
#   facet_grid(d ~ dtau_type + distribution)
