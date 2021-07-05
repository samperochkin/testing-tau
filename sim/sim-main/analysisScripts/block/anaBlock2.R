# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)

dt <- rbindlist(lapply(fii[11],fread))

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)), "NN" = .N),
          by=c("n","d","design","S","Sh","norm","dtau")]
dt3 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
          by=c("n","d","design","S","Sh","norm","dtau","isShPd")]

all(dt2$N == 1)
all(dt2$NN == 2500)
all(dt3$isShPd)
rm(dt3)

## Levels
# S = Sh
xdt <- dcast(dt2[norm == "Euclidean" & S == "Sh" & dtau == 0],
             formula = Sh + d ~ design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "Sh" & dtau == 0],
             formula = Sh + d ~ design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == 0],
             formula = Sh + d ~ design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == 0],
             formula = Sh + d ~ design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



## Powers
## Single .01

# S = Sh
xdt <- dcast(dt2[norm == "Euclidean" & S == "Sh" & dtau == .05],
             formula = Sh + d ~ design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "Sh" & dtau == .05],
             formula = Sh + d ~ design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" & dtau == .05],
             formula = Sh + d ~ design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & dtau == .05],
             formula = Sh + d ~ design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)


xdt1 <- dcast(dt2[Sh %in% c("ShP","ShJ") & dtau %in% c(0,.05)],
             formula = S + norm + Sh + d ~ dtau + design + n,
             value.var = "level")
xdt1
print(xtable(xdt1[,-c(1,2)],digits=1),include.rownames = F)


xdt2 <- dcast(dt2[Sh %in% c("SbP","SbJ") & dtau %in% c(0,.05)],
             formula = S + norm + Sh + d ~ dtau + design + n,
             value.var = "level")
xdt2
print(xtable(xdt2[,-c(1,2)],digits=1),include.rownames = F)


rbind(xdt1,xdt2)
rbind(xdt1,xdt2)[grepl("J",Sh)]
print(xtable(rbind(xdt1,xdt2)[,-c(1,2)],digits=1),include.rownames = F)





#### initial test


dt2[, structured := grepl("b",dt2$Sh)]
dt3[, structured := grepl("b",dt3$Sh)]

dt2[Sh %in% c("ShP","SbP"), Sh := "P"]
dt2[Sh %in% c("ShJ","SbJ"), Sh := "J"]
dt3[Sh %in% c("ShP","SbP"), Sh := "P"]
dt3[Sh %in% c("ShJ","SbJ"), Sh := "J"]

dt2[,N := NULL]
dt2[,NN := NULL]
dt3[,N := NULL]

dt4 <- dt3[isShPd == T,]
dt4[,isShPd := NULL]

dt4 <- rbind(dt4[structured == F & S == "Sh"],
             dt2[!(structured == F & S == "Sh")])

xdt <- dcast(dt4[dtau == 0],
             formula = S + norm + Sh + d ~ structured + design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)


xdt <- dcast(dt4[dtau == .1],
             formula = S + norm + Sh + d ~ structured + design + n,
             value.var = "level")
xdt
print(xtable(xdt[,-c(1:2)],digits=1),include.rownames = F)




dt4 <- dt3[isShPd == T,]
dt4[,isShPd := NULL]

dt4 <- rbind(dt4[structured == F & S == "Sh"],
             dt2[!(structured == F & S == "Sh")])

xdt <- dcast(dt4,
             formula = S + norm + Sh + d ~ structured + design + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)


xdt <- dcast(dt4[dtau == .1],
             formula = S + norm + Sh + d ~ structured + design + n,
             value.var = "level")
xdt
print(xtable(xdt[,-c(1:2)],digits=1),include.rownames = F)
