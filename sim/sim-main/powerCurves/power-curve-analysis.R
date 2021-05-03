

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)



# Load results ------------------------------------------------------------

full.grid <- fread("sim/sim-main/powerCurves/full_grid_1.csv")


# Plots -------------------------------------------------------------------


# single - normal
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "normal"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)


# column - normal
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "normal"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)


# single - supremum
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & norm == "Supremum"],
       aes(x=epsilon, y=power, col=distribution, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# single - euclidean
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & norm == "Euclidean"],
       aes(x=epsilon, y=power, col=distribution, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# column - supremum
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & norm == "Supremum"],
       aes(x=epsilon, y=power, col=distribution, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# column - euclidean
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & norm == "Euclidean"],
       aes(x=epsilon, y=power, col=distribution, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)



# single - cauchy
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "cauchy"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# column - cauchy
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "cauchy"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

