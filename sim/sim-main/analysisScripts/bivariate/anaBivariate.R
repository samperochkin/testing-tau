# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("sim/sim-main/results/ulaval-server-results",full.names = T)
fii <- grep(".csv", fii,value = T)
fii

dt <- rbindlist(lapply(fii[c(3:5)], function(fi){
  fread(fi)[Sh == "SbJ"]
  }), fill=TRUE)
all(dt$isShPd)

dt$s_info

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

table(dt$n)
dt[,.N,.(S,Sh,norm,d,pvalue_method,n)]

dt_size <- dt[dtau_type == "none", .(size = mean(decision)), .(S,Sh,norm,n,d,tau)]
dt_power <- dt[dtau_type != "none",.(power = mean(decision)), .(S,Sh,norm,n,d,tau,dtau,dtau_type)]

library(ggplot2)

ggplot(dt_size, aes(x = n, y = size, col=S, linetype=norm)) +
  geom_line() +
  facet_grid(tau~d, scales = "free_x")

ggplot(dt_power[dtau == .1], aes(x = n, y = power, col=S, linetype=norm)) +
  geom_line() +
  facet_grid(tau~d+dtau_type, scales = "free_x")


table(dt[S == "I"]$pvalue_method)
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
