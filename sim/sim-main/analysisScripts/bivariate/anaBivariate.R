# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

fii <- list.files("sim/sim-main/results/ulaval-server-results",full.names = T)
fii <- grep(".csv", fii,value = T)
fii

fii <- c(fii[c(3:5)], list.files("sim/sim-main/results/extra-for-presentation",full.names = T))


dt <- rbindlist(lapply(fii, function(fi){
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

dt_size <- dt[dtau_type == "none", .(size = 100*mean(decision)), .(S,Sh,norm,n,d,tau)]
dt_power <- dt[dtau_type != "none",.(power = 100*mean(decision)), .(S,Sh,norm,n,d,tau,dtau,dtau_type)]

library(ggplot2)

ggplot(dt_size, aes(x = n, y = size, col=S, linetype=norm)) +
  geom_line() +
  facet_grid(tau~d, scales = "free_x")

ggplot(dt_power[dtau == .1], aes(x = n, y = power, col=S, linetype=norm)) +
  geom_line() +
  facet_grid(tau~d+dtau_type, scales = "free_x")



ds <- c(15)
dtau_single <- .1
dtau_column <- .1

## Levels
xdt <- dcast(dt_size[norm == "Euclidean" & d %in% ds],
             formula = S + d ~ tau + n,
             value.var = "size")
xdt

print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt_size[norm == "Supremum" & d %in% ds],
             formula = S + d ~ tau + n,
             value.var = "size")
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-c(1,2)],digits=1),include.rownames = F)


## Power (single)
xdt <- dcast(dt_power[norm == "Euclidean" &  d %in% ds & dtau == dtau_single & dtau_type == "single"],
             formula = S + d ~ tau + n,
             value.var = "power")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt_power[norm == "Supremum" &  d %in% ds & dtau == dtau_single & dtau_type == "single"],
             formula = S + d ~ tau + n,
             value.var = "power")
xdt
print(xtable(xdt,digits=1),include.rownames = F)


## Power (column)
xdt <- dcast(dt_power[norm == "Euclidean" &  d %in% ds & dtau == dtau_column & dtau_type == "column"],
             formula = S + d ~ tau + n,
             value.var = "power")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

xdt <- dcast(dt_power[norm == "Supremum" &  d %in% ds & dtau == dtau_column & dtau_type == "column"],
             formula = S + d ~ tau + n,
             value.var = "power")
xdt
print(xtable(xdt,digits=1),include.rownames = F)



## New test
dt_size_new <- dt[S %in% c("Sh-p", "Sh-d") & dtau_type == "none", .(min_pvalue = min(pvalue), S = "new"), .(ID,Sh,norm,n,d,tau)]
dt_size_new[, decision := min_pvalue < .05/2]
dt_size_new <- dt_size_new[, .(size = 100*mean(decision)), .(Sh,S,norm,n,d,tau)]
dt_size_new
dt_size <- rbind(dt_size, dt_size_new)
rm(dt_size_new)

dt_power_new <- dt[S %in% c("Sh-p", "Sh-d") & dtau_type != "none", .(min_pvalue = min(pvalue), S = "new"), .(ID,Sh,norm,n,d,tau,dtau,dtau_type)]
dt_power_new[, decision := min_pvalue < .05/2]
dt_power_new <- dt_power_new[, .(power = 100*mean(decision)), .(Sh,S,norm,n,d,tau,dtau,dtau_type)]
dt_power_new
dt_power <- rbind(dt_power, dt_power_new)
rm(dt_power_new)


## Levels
xdt <- dcast(dt_size[norm == "Euclidean" & d %in% ds],
             formula = S + d ~ tau + n,
             value.var = "size")
xdt

print(xtable(xdt,digits=1),include.rownames = F)

## Power (single)
xdt <- dcast(dt_power[norm == "Euclidean" &  d %in% ds & dtau == dtau_single & dtau_type == "single"],
             formula = S + d ~ tau + n,
             value.var = "power")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

## Power (column)
xdt <- dcast(dt_power[norm == "Euclidean" &  d %in% ds & dtau == dtau_column & dtau_type == "column"],
             formula = S + d ~ tau + n,
             value.var = "power")
xdt
print(xtable(xdt,digits=1),include.rownames = F)





# d=5
## Levels
xdt1 <- dcast(dt_size[norm == "Euclidean" & d == 5 & tau == .6],
              formula = S ~ n,
              value.var = "size")
xdt1

print(xtable(xdt1,digits=1),include.rownames = F)

## Power (single)
xdt2 <- dcast(dt_power[norm == "Euclidean" & d == 5 & tau == .6 & dtau == dtau_single & dtau_type == "single"],
              formula = S ~ n,
              value.var = "power")
xdt2
print(xtable(xdt2,digits=1),include.rownames = F)

## Power (column)
xdt3 <- dcast(dt_power[norm == "Euclidean" & d == 5 & tau == .6 & dtau == dtau_column & dtau_type == "column"],
              formula = S ~ n,
              value.var = "power")
xdt3
print(xtable(xdt3,digits=1),include.rownames = F)

print(xtable(cbind(xdt1,xdt2[,-1],xdt3[,-1]),digits=1),include.rownames = F)





## Levels
xdt1 <- dcast(dt_size[norm == "Euclidean" & d == 15 & tau == .6],
             formula = S ~ n,
             value.var = "size")
xdt1

print(xtable(xdt1,digits=1),include.rownames = F)

## Power (single)
xdt2 <- dcast(dt_power[norm == "Euclidean" & d == 15 & tau == .6 & dtau == dtau_single & dtau_type == "single"],
             formula = S ~ n,
             value.var = "power")
xdt2
print(xtable(xdt2,digits=1),include.rownames = F)

## Power (column)
xdt3 <- dcast(dt_power[norm == "Euclidean" & d == 15 & tau == .6 & dtau == dtau_column & dtau_type == "column"],
             formula = S ~ n,
             value.var = "power")
xdt3
print(xtable(xdt3,digits=1),include.rownames = F)

print(xtable(cbind(xdt1,xdt2[,-1],xdt3[,-1]),digits=1),include.rownames = F)


