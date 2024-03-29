# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)

dt <- rbindlist(lapply(fii[3:5],fread))
all(dt$isShPd)

# dt[, s_info := NULL]

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
dt3 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","s_info")]

all(dt2$N == 1)
all(dt2$NN == 2500)



xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & dtau == 0],
             formula = s_info + Sh + d ~ tau + n,
             value.var = "N")
xdt
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & dtau == 0],
             formula = s_info + Sh + d ~ tau + n,
             value.var = "level")
xdt





## Levels
# S = Sh
xdt <- dcast(dt2[norm == "Euclidean" & S == "Sh" & dtau == 0],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "Sh" & dtau == 0],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)



# Sh = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == 0],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == 0],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)



# ## Powers
# ## Single .1
# 
# # S = I
# xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .1 & dtau_type == "single"],
#              formula = Sh + d ~ tau + n,
#              value.var = "level")
# xdt
# print(xtable(xdt,digits=1),include.rownames = F)
# print(xtable(xdt[,-c(1)],digits=1),include.rownames = F)
# 
# xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .1 & dtau_type == "single"],
#              formula = Sh + d ~ tau + n,
#              value.var = "level")
# xdt
# print(xtable(xdt,digits=1),include.rownames = F)
# print(xtable(xdt[,-c(1)],digits=1),include.rownames = F)



## Powers
## Single .2

# S = Sh
xdt <- dcast(dt2[norm == "Euclidean" & S == "Sh" & dtau == .2 & dtau_type == "single"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "Sh" & dtau == .2 & dtau_type == "single"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .2 & dtau_type == "single"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .2 & dtau_type == "single"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)





## Powers
## column .1

# S = Sh
xdt <- dcast(dt2[norm == "Euclidean" & S == "Sh" & dtau == .1 & dtau_type == "column"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "Sh" & dtau == .1 & dtau_type == "column"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .1 & dtau_type == "column"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .1 & dtau_type == "column"],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)
