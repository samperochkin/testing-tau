

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)



# Load results ------------------------------------------------------------

full.grid1 <- fread("sim/sim-main/powerCurves/full_grid_1.csv")
full.grid1[,large_n := 10000]
full.grid1[,N := 10000]

full.grid2 <- fread("sim/sim-main/powerCurves/full_grid_2.csv")
full.grid2[,large_n := 50000]
full.grid2[,N := 50000]

full.grid12 <- rbindlist(list(full.grid1,full.grid2))


# Plots -------------------------------------------------------------------

al <- .05
ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     d == 50 & S == "Sh" & dtau_type == "single"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype = as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                   S == "I" & dtau_type == "single"],
      aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     S == "Sh" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     S == "I" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     S == "Sh" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)


al <- .001
ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     d == 50 & S == "Sh" & dtau_type == "single"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype = as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     S == "I" & dtau_type == "single"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     S == "Sh" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     S == "I" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "normal" &
                     S == "Sh" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)





al <- .05
ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     d == 50 & S == "Sh" & dtau_type == "single"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype = as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     S == "I" & dtau_type == "single"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     S == "Sh" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     S == "I" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     S == "Sh" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)


al <- .001
ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     d == 50 & S == "Sh" & dtau_type == "single"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype = as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     S == "I" & dtau_type == "single"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     S == "Sh" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     S == "I" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)

ggplot(full.grid12[round(alpha,3)==al & distribution == "cauchy" &
                     S == "Sh" & dtau_type == "column"],
       aes(x=epsilon, y=power, col=as.factor(N), linetype=as.factor(N))) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_grid(norm+d~tau+dtau_type)
