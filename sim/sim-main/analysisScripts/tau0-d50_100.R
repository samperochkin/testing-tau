library(data.table)
library(ggplot2)
library(xtable)

fii <- list.files("sim-main/results/",full.names = T)
fii

dt <- rbindlist(lapply(fii[2:4],fread))

dt <- rbindlist(c(list(dt),lapply(fii[c(7,8)],fread)))
dt$isShPsd <- NA
dt$rankSh <- NA
dt <- rbindlist(c(list(dt),lapply(fii[c(5,6)],fread)))
dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))

table(dt[Sh == "SbJ",]$isShPd == F)
table(is.na(dt[Sh == "SbJ"]$pvalue))
table(is.na(dt[Sh == "SbJ"]$pvalue))
table(is.na(dt[Sh == "SbJ"]$pvalue))


dt[Sh == "SbJ" & is.na(pvalue),]
dt[Sh == "SbJ", .(mNA = mean(is.na(pvalue))), .(S, norm,n)]
dt[Sh == "SbJ" & norm == "Supremum", .(mNA = mean(is.na(pvalue))), .(S,n,d)]
dt[Sh == "SbJ" & norm == "Supremum" & d %in% c(50,100), .(mNA = mean(is.na(pvalue))), .(S,n,d)]
dt[Sh == "SbJ" & norm == "Supremum" & d %in% c(50,100), .(mNA = mean(is.na(pvalue))), .(S,n,d,tau)]
dt[Sh == "SbJ" & norm == "Supremum" & d %in% c(50,100) & tau == 0, .(mNA = mean(is.na(pvalue))), .(S,n,d,dtau)]


dt[Sh == "SbJ" & norm == "Supremum" & d %in% c(50,100) & tau == 0, .(mNA = mean(is.na(pvalue)), mPD = mean(!isShPd)), .(S,n,d,dtau)]
