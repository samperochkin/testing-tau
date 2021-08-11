library(data.table)
library(ggplot2)

dt <- fread("sim/sim-main/results/dt_exch_full.csv")



# H0

n0 <- 150
d0 <- unique(dt$d)
tau0 <- .3
# dtau0 <- c(0,.1,.2)
Sh0 <- "ShJ"
S0 <- "I"

# dt2 <- dt[n %in% n0 & Sh %in% Sh0 & tau %in% tau0 &
#             (dtau_type == "none" | (dtau_type == "single" & dtau == .2) | (dtau_type == "column" & dtau == .1))]
dt2 <- dt[n %in% n0 & d %in% d0 & Sh %in% Sh0 & S %in% S0 & tau %in% tau0 &
            ((dtau_type == "single" & dtau == .2) | (dtau_type == "column" & dtau == .1))]

ggplot(dt2, aes(x=d, y=rejection_rate, col=S, linetype=norm)) +
  theme_light() +
  geom_point(cex=1) +
  geom_line() +
  coord_cartesian(ylim=c(0,100), xlim=c(0,105)) +
  # scale_x_continuous(breaks = c(5,25,50,100)) +
  facet_grid(dtau_type~distribution)

dt2 <- dt[n %in% n0 & d %in% d0 & Sh %in% Sh0 & S %in% S0 & tau %in% tau0 &
            dtau_type == "none"]

ggplot(dt2, aes(x=d, y=rejection_rate, col=S, linetype=norm)) +
  theme_light() +
  geom_point(cex=1) +
  geom_line() +
  geom_hline(yintercept = 5, col="green",alpha=.25) +
  coord_cartesian(ylim=c(0,6), xlim=c(0,105)) +
  facet_grid(~distribution)



n0 <- c(50,100,150)
d0 <- 15
tau0 <- .3
# dtau0 <- c(0,.1,.2)
Sh0 <- "ShJ"
S0 <- "I"

dt2 <- dt[n %in% n0 & d %in% d0 & Sh %in% Sh0 & S %in% S0 & tau %in% tau0 &
            ((dtau_type == "single" & dtau == .2) | (dtau_type == "column" & dtau == .1))]

ggplot(dt2, aes(x=n, y=rejection_rate, col=S, linetype=norm)) +
  theme_light() +
  geom_point(cex=1) +
  geom_line() +
  coord_cartesian(ylim=c(0,100), xlim=c(45,155)) +
  facet_grid(dtau_type~distribution)

dt2 <- dt[n %in% n0 & d %in% d0 & Sh %in% Sh0 & S %in% S0 & tau %in% tau0 &
            dtau_type == "none"]

ggplot(dt2, aes(x=n, y=rejection_rate, col=S, linetype=norm)) +
  theme_light() +
  geom_point(cex=1) +
  geom_line() +
  geom_hline(yintercept = 5, col="green",alpha=.25) +
  coord_cartesian(ylim=c(0,6), xlim=c(45,155)) +
  scale_x_continuous(breaks = c(50,100,150)) +
  facet_grid(~distribution)
#









# H0-star

n0 <- 150
d0 <- unique(dt$d)
tau0 <- .3
# dtau0 <- c(0,.1,.2)
Sh0 <- "SbJ"

# dt2 <- dt[n %in% n0 & Sh %in% Sh0 & tau %in% tau0 &
#             (dtau_type == "none" | (dtau_type == "single" & dtau == .2) | (dtau_type == "column" & dtau == .1))]
dt2 <- dt[n %in% n0 & d %in% d0 & Sh %in% Sh0 & tau %in% tau0 &
            ((dtau_type == "single" & dtau == .2) | (dtau_type == "column" & dtau == .1))]

ggplot(dt2, aes(x=d, y=rejection_rate, col=S, linetype=norm)) +
  theme_light() +
  geom_line() +
  facet_grid(dtau_type~distribution)

dt2 <- dt[n %in% n0 & d %in% d0 & Sh %in% Sh0 & tau %in% tau0 &
            dtau_type == "none"]

ggplot(dt2, aes(x=d, y=rejection_rate, col=S, linetype=norm)) +
  theme_light() +
  geom_line() +
  ylim(c(0,10)) +
  facet_grid(~distribution)



n0 <- c(50,100,150)
d0 <- 25
tau0 <- .3
# dtau0 <- c(0,.1,.2)
Sh0 <- "SbJ"

dt2 <- dt[n %in% n0 & d %in% d0 & Sh %in% Sh0 & tau %in% tau0 &
            ((dtau_type == "single" & dtau == .2) | (dtau_type == "column" & dtau == .1))]

ggplot(dt2, aes(x=n, y=rejection_rate, col=S, linetype=norm)) +
  theme_light() +
  geom_point(cex=.75) +
  geom_line() +
  facet_grid(dtau_type~distribution)

dt2 <- dt[n %in% n0 & d %in% d0 & Sh %in% Sh0 & tau %in% tau0 &
            dtau_type == "none"]

ggplot(dt2, aes(x=n, y=rejection_rate, col=S, linetype=norm)) +
  theme_light() +
  geom_point(cex=1) +
  geom_line() +
  ylim(c(0,10)) +
  facet_grid(~distribution)
