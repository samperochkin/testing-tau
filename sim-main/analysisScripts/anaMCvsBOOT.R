library(data.table)
library(ggplot2)
library(xtable)

fii <- list.files("sim-main/results/",full.names = T)

dt <- rbindlist(lapply(fii[c(2,3,4,7,8)],fread))
dt$isShPsd <- NA
dt$rankSh <- NA

dt <- rbindlist(c(list(dt),lapply(fii[c(5,6)],fread)))
dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]
dt[S == "Sh" & Sh == "SbJ" & norm == "Euclidean" & d %in% c(50,100), decision := pvalue > .95] # mistake in sims

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","pvalue_method")]





# level
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "N")
xdt
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)




# Sh = I -- single
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "N")
xdt
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)




# S = Sh -- column
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "N")
xdt
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

