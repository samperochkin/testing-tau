# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)

dt <- rbindlist(lapply(fii[10],fread))

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))


dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)), "NN" = .N),
          by=c("n","d","design","S","Sh","norm","dtau")]

dt2 <- dt2[grepl("b",Sh)]

## Levels
xdt1 <- dcast(dt2[S == "Sh" & dtau == 0],
             formula = Sh + d ~ norm + design + n,
             value.var = "level")
xdt1
print(xtable(xdt1,digits=1),include.rownames = F)

xdt2 <- dcast(dt2[S == "I" & dtau == 0],
              formula = Sh + d ~ norm + design + n,
              value.var = "level")
print(xtable(xdt1,digits=1),include.rownames = F)

xdt <- rbind(xdt1,xdt2)
xdt <- cbind(NA,xdt[,2:8],NA,xdt[,9:14])
print(xtable(xdt,digits=1),include.rownames = F, include.colnames = F)


## Powers
xdt1 <- dcast(dt2[S == "Sh" & dtau == .1],
              formula = Sh + d ~ norm + design + n,
              value.var = "level")
xdt1
print(xtable(xdt1,digits=1),include.rownames = F)

xdt2 <- dcast(dt2[S == "I" & dtau == .1],
              formula = Sh + d ~ norm + design + n,
              value.var = "level")
print(xtable(xdt1,digits=1),include.rownames = F)

xdt <- rbind(xdt1,xdt2)
xdt <- cbind(NA,xdt[,2:8],NA,xdt[,9:14])
print(xtable(xdt,digits=1),include.rownames = F, include.colnames = F)


