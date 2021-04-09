setwd("sim/sim-main")

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(xtable)



# Load simulations and define parameters ----------------------------------

dt <- fread("dt_main_local_5.csv")
# dt <- fread("dt_main_local_1.csv")
dt[, epsilon := dtau] # watch out here
unique(dt$epsilon)

dt[,decision := pvalue < .05]
dt$Sh <- factor(dt$Sh, levels = c("ShP","ShJ","SbP","SbJ"))
dt$S <- factor(dt$S, levels = c("Sh","I"))

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("epsilon","tau","n","d","S","Sh","norm","dtau_type")]


ggplot(dt2, aes(x=n, y=level, col=norm, linetype=norm)) +
  theme_light() +
  geom_line() +
  geom_abline(slope=0, intercept = 5, col = "black", alpha = .25) +
  ylim(c(0,10)) +
  facet_grid(dtau_type+epsilon+tau~Sh+S)

ggplot(dt2[norm == "Supremum"], aes(x=n, y=level, linetype=S, col=as.factor(tau))) +
  theme_light() +
  geom_line() +
  geom_abline(slope=0, intercept = 5, col = "black", linetype=2) +
  ylim(c(0,10)) +
  facet_grid(dtau_type+epsilon~Sh)


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
ggplot(dt[epsilon == .25 & tau == .25 & dtau_type == "single"],
       aes(sample=pvalue, col=norm, linetype=norm)) +
  theme_light() +
  geom_qq(distribution=stats::qunif, size = .25) +
  geom_abline(slope=1, intercept = 0, col = "black") +
  ylim(c(0,1)) +
  facet_grid(S~Sh)


ggplot(dt, aes(sample=pvalue, col=norm, linetype=norm)) +
  theme_light() +
  geom_qq(distribution=stats::qunif, size = .25) +
  geom_abline(slope=1, intercept = 0, col = "black") +
  ylim(c(0,1)) +
  facet_grid(n+dtau_type+epsilon+tau~Sh+S)
