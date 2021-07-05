setwd("sim/sim-main")

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(xtable)



# Load simulations and define parameters ----------------------------------

dt <- fread("dt_main_local_6.csv")
# dt <- fread("dt_main_local_1.csv")
dt[, epsilon := dtau] # watch out here
unique(dt$epsilon)

dt[,decision := pvalue < .05]
dt$Sh <- factor(dt$Sh, levels = c("ShP","ShJ","SbP","SbJ"))
dt$S <- factor(dt$S, levels = c("Sh","I"))

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("epsilon","tau","n","d","S","Sh","norm","dtau_type")]


ggplot(dt2[tau == .25 & epsilon == .5 & dtau_type == "single"],
       aes(x=n, y=level, col=S, linetype=norm)) +
  theme_light() +
  geom_line() +
  geom_abline(slope=0, intercept = 5, col = "black", alpha = .25, lty=1) +
  coord_cartesian(ylim=c(0,50)) +
  facet_grid(d~Sh)

ggplot(dt2[tau == .25 & epsilon == .5 & dtau_type == "column"],
       aes(x=n, y=level, col=S, linetype=norm)) +
  theme_light() +
  geom_line() +
  geom_abline(slope=0, intercept = 5, col = "black", alpha = .25, lty=1) +
  coord_cartesian(ylim=c(0,50)) +
  facet_grid(d~Sh)

ggplot(dt2[tau == .5],
       aes(x=n, y=level, col=S, linetype=norm)) +
  theme_light() +
  geom_line() +
  geom_abline(slope=0, intercept = 5, col = "black", alpha = .25, lty=1) +
  coord_cartesian(ylim=c(0,50)) +
  facet_grid(dtau_type+Sh~epsilon+d)


# Sh = I
xdt <- dcast(dt2[Sh %in% c("ShP","ShJ","SbP","SbJ")],
             formula = dtau_type+epsilon+tau+d ~ norm+Sh+S+n,
             value.var = "level")
xdt

xdt <- dcast(dt2[Sh %in% c("ShP","ShJ","SbP","SbJ")],
             formula = dtau_type+epsilon+tau+n+d+Sh ~ S+norm,
             value.var = "level")
xdt
print(xtable(xdt,digits=2),include.rownames = F)




# QQ
ggplot(dt[epsilon == 1.5 & tau == .5 & dtau_type == "column"],
       aes(sample=pvalue, col=norm, linetype=norm)) +
  theme_light() +
  geom_qq(distribution=stats::qunif, size = .25) +
  geom_abline(slope=1, intercept = 0, col = "black") +
  ylim(c(0,1)) +
  facet_grid(S+Sh~d+n)

xdt <- dcast(dt2[tau == .5 & dtau_type == "column" & d == 10],
             formula = norm+Sh+S+n~epsilon,
             value.var = "level")
xdt
