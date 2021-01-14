library(data.table)
library(ggplot2)
library(xtable)

dt <- fread("sim-main/results/dt_main_app.csv")

ggplot(dt, aes(col = norm)) + 
  stat_qq(aes(sample=pvalue), distribution = qunif) +
  #scale_y_continuous(breaks = seq(.25, .75, by = .5)) +
  ylim(0,1) +
  facet_grid(S~Sh)


dt$n <- 65
dt$d <- 18

dt[,decision := pvalue < .05]
dt$Sh <- factor(dt$Sh, levels = c("ShP","ShJ"))
dt$S <- factor(dt$S, levels = c("Sh","I"))

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("n","d","S","Sh","norm")]

# Sh = I
xdt <- dcast(dt2[Sh %in% c("ShP","ShJ")],
             formula = norm~Sh+S,
             value.var = "N")
xdt

xdt <- dcast(dt2[Sh %in% c("ShP","ShJ")],
             formula = Sh ~ S+norm,
             value.var = "level")
xdt
print(xtable(xdt,digits=2),include.rownames = F)

xdt <- dcast(dt2[Sh %in% c("ShP","ShJ") & S == "I"],
             formula = Sh ~ norm,
             value.var = "level")
xdt
print(xtable(xdt,digits=2),include.rownames = F)