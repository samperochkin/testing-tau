

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(xtable)



# Load simulations and define parameters ----------------------------------

dt <- fread("dt_main_local_2")
head(dt)

# n <- 500
d <- 3
tau <- .25
epsilon <- .75









library(data.table)
library(ggplot2)
library(xtable)

dt <- fread("sim/sim-main/dt_main_local_2.csv")
# dt <- fread("dt_main_local_1.csv")
dt[, epsilon := dtau] # watch out here
unique(dt$epsilon)

dt[,decision := pvalue < .05]
dt$Sh <- factor(dt$Sh, levels = c("ShP","ShJ","SbP","SbJ"))
dt$S <- factor(dt$S, levels = c("Sh","I"))

dt2 <- dt[,.("power" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("epsilon","tau","n","d","S","Sh","norm","dtau_type")]


ggplot(dt2, aes(x=n, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_abline(slope=0, intercept = 5, col = "black", alpha = .25) +
  ylim(c(0,100)) +
  facet_grid(epsilon+tau~Sh)

ggplot(dt2[norm == "Supremum"], aes(x=n, y=power, linetype=S, col=as.factor(tau))) +
  theme_light() +
  geom_line() +
  geom_abline(slope=0, intercept = 5, col = "black", linetype=2) +
  ylim(c(0,65)) +
  facet_grid(dtau_type+epsilon~Sh)

ggplot(dt2[norm == "Supremum" & epsilon == 1 & dtau_type == "single"],
       aes(x=n, y=power, linetype=S, col=as.factor(tau))) +
  theme_light() +
  geom_line() +
  geom_abline(slope=0, intercept = 5, col = "black", linetype=2) +
  ylim(c(0,65)) +
  facet_grid(~Sh)

# Sh = I
xdt <- dcast(dt2[Sh %in% c("ShP","ShJ","SbP","SbJ")],
             formula = epsilon+tau+d ~ norm+Sh+S+n,
             value.var = "level")
xdt

xdt <- dcast(dt2[Sh %in% c("ShP","ShJ","SbP","SbJ")],
             formula = epsilon+tau+n+d+Sh ~ S+norm,
             value.var = "level")
xdt
print(xtable(xdt,digits=2),include.rownames = F)

xdt <- dcast(dt2[Sh %in% c("ShP","ShJ","SbP","SbJ") & S == "I"],
             formula = n+d+Sh ~ norm,
             value.var = "level")
xdt
print(xtable(xdt,digits=2),include.rownames = F)