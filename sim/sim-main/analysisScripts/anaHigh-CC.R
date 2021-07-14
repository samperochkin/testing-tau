# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("/store/samuel/testing-tau-extra/",full.names = T)

dt <- rbindlist(lapply(fii[1:6],fread))

dt[,decision := pvalue < .05]

table(dt$dtau_type)
dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
table(dt$S)
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
table(dt$Sh)
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

#### À voir
#dt[isShPd == F, pvalue := NA]

# dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)), "NN" = .N),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","distribution")]

all(dt2$N == 1)
all(dt2$NN == 1000)


### CURVES

ggplot(dt2[dtau == 0 & S %in% c("I", "Sh")], aes(x=n, y=level, col=norm, linetype=S)) +
  geom_line() +
  facet_grid(d+distribution~tau)


ggplot(dt2[dtau_type == "single" & S %in% c("I", "Sh")], aes(x=n, y=level, col=norm, linetype=S)) +
  geom_line() +
  facet_grid(d+distribution~tau+dtau)

ggplot(dt2[dtau_type == "column" & S %in% c("I", "Sh")], aes(x=n, y=level, col=norm, linetype=S)) +
  geom_line() +
  facet_grid(d+distribution~tau+dtau)

#









### TABLES


## Levels
# Sh = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == 0 & distribution == "clayton"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == 0 & distribution == "gumbel"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == 0 & distribution == "t4"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt

print(xtable(xdt,digits=1),include.rownames = F)



xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == 0 & distribution == "clayton"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == 0 & distribution == "gumbel"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == 0 & distribution == "t4"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



## Powers
## Single .1

# Sh = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .1 & dtau_type == "single" & distribution == "clayton"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .1 & dtau_type == "single" & distribution == "gumbel"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .1 & dtau_type == "single" & distribution == "t4"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .1 & dtau_type == "single" & distribution == "clayton"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .1 & dtau_type == "single" & distribution == "gumbel"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .1 & dtau_type == "single" & distribution == "t4"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)

