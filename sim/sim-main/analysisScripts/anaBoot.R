# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)

dt <- rbindlist(lapply(fii[c(8,9,1,2)],fread))

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

#### À voir
#dt[isShPd == F, pvalue := NA]

# dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)), "NN" = .N),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type")]

all(dt2$N == 1)
all(dt2$NN == 2500)

## Levels

# Sh = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == 0],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == 0],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



## Powers
## Single .1

# Sh = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .1 & dtau_type == "single"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .1 & dtau_type == "single"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)



## Powers
## Single .2

# Sh = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .2 & dtau_type == "single"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .2 & dtau_type == "single"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)





## Powers
## column .1

# Sh = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .1 & dtau_type == "column"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .1 & dtau_type == "column"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
