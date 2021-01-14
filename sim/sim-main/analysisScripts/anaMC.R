# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)

dt <- rbindlist(lapply(fii[6:7],fread))

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

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




## Levels
# S = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5,15) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5,15) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5,15) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt2[norm == "Supremum" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)






# dtau = .1 -- single
# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt2[norm == "Supremum" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)





# dtau = .2 -- single
# S = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
# 
xdt <- dcast(dt2[norm == "Supremum" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)





# dtau = .1 -- column
# S = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

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



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt2[norm == "Supremum" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

####
# For bootstrap with same dimensions (d=5,15,25), see files named dt_main_low_3 and 4
# For higher dimensions (d=50,100), see files named dt_main_boot_3 and 4 (bootstrap only)
####








# dtau = .2 -- column
# S = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .2 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .2 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,150,250) & d %in% c(5) &
                   dtau == .2 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
# 
xdt <- dcast(dt2[norm == "Supremum" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("ShP","ShJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)

####
# For bootstrap with same dimensions (d=5,15,25), see files named dt_main_low_3 and 4
# For higher dimensions (d=50,100), see files named dt_main_boot_3 and 4 (bootstrap only)
####









### SB ### STRUCTURED
## Levels
# S = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt2[norm == "Supremum" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)





# dtau = .2 -- single
# S = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt2[norm == "Supremum" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



# dtau = .1 -- column
# S = Sh
xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt3[norm == "Euclidean" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt3[norm == "Supremum" & S == "Sh" & isShPd == T &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



# S = I
xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "N")
xdt

xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
# 
xdt <- dcast(dt2[norm == "Supremum" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" &
                   tau %in% c(0,.3,.6) & Sh %in% c("SbP","SbJ")],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

####
# For higher dimensions (d=50,100), see files named dt_main_high
####



