# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)

dt <- rbindlist(lapply(fii[1:3],fread))
dt[, s_info := NULL]
dt <- rbindlist(c(list(dt),lapply(fii[6:7],fread)))
dt$isShPsd <- NA
dt$rankSh <- NA

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]
dt[S == "Sh" & Sh == "SbJ" & norm == "Euclidean" & d %in% c(50,100), decision := pvalue > .95] # mistake in sims

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

#### À voir
#dt[isShPd == F, pvalue := NA]

# dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type")]
dt3 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","isShPd")]
#dt2[N < .9, level := NA]
# dt2[dtau == 0 & pvalue_method != "bootstrap" & S == "I" & Sh == "ShP" & norm == "Euclidean",]
dt2[dtau == 0 & S == "I" & Sh == "ShP" & norm == "Euclidean",]
dt3[dtau == 0 & S == "I" & Sh == "ShP" & norm == "Euclidean",]

# Sh = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .1 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt
# dt3[N < 1000,]
# dt3[N < 1000, level := NA]
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
# xdt <- dcast(dt2[norm == "Supremum" & S == "Sh" &
#                    n %in% c(50,150,250) & d %in% c(5,15) &
#                    dtau == 0 & tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
#              formula = Sh + d ~ tau + n,
#              value.var = "level")
# xdt
# print(xtable(xdt,digits=1),include.rownames = F)



# Sh = I -- single
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)




# S = Sh -- column
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)


xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)





#### SB

# Sh = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .1 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt
# dt3[N < 1000,]
# dt3[N < 1000, level := NA]
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
# xdt <- dcast(dt2[norm == "Supremum" & S == "Sh" &
#                    n %in% c(50,150,250) & d %in% c(5,15) &
#                    dtau == 0 & tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
#              formula = Sh + d ~ tau + n,
#              value.var = "level")
# xdt
# print(xtable(xdt,digits=1),include.rownames = F)



# Sh = I -- single
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)




# Sh = I -- column
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)


xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25,50,100) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
