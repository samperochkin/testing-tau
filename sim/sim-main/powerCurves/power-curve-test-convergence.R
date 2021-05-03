

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)



# Load results ------------------------------------------------------------

full.grid2 <- fread("sim/sim-main/powerCurves/full_grid_2.csv")
full.grid2[,large_n := 10000]
full.grid2[,N := 10000]

full.grid3 <- fread("sim/sim-main/powerCurves/full_grid_3.csv")
full.grid3[,large_n := 50000]
full.grid3[,N := 50000]

full.grid23 <- rbindlist(list(full.grid2,full.grid3))


# Plots -------------------------------------------------------------------


# single - supremum
al <- .05
ggplot(full.grid23[round(alpha,3)==al & dtau_type == "single" & norm == "Supremum"],
       aes(x=epsilon, y=power, col=large_n, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# single - euclidean
al <- .05
ggplot(full.grid23[round(alpha,3)==al & dtau_type == "single" & norm == "Euclidean"],
       aes(x=epsilon, y=power, col=large_n, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# column - supremum
al <- .05
ggplot(full.grid23[round(alpha,3)==al & dtau_type == "column" & norm == "Supremum"],
       aes(x=epsilon, y=power, col=large_n, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# column - euclidean
al <- .05
ggplot(full.grid23[round(alpha,3)==al & dtau_type == "column" & norm == "Euclidean"],
       aes(x=epsilon, y=power, col=large_n, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)



