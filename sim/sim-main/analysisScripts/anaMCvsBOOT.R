library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)

dt <- rbindlist(lapply(fii[c(8,9)],fread))
dt$isShPsd <- NA
dt$rankSh <- NA

dt <- rbindlist(c(list(dt),lapply(fii[6:7],fread)))
dt <- dt[S == "I",]

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","pvalue_method")]





# level
xdt1 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == 0 & pvalue_method == "Monte Carlo" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = d ~ tau + n,
             value.var = "level")
xdt2 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 & pvalue_method == "bootstrap" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
cbind(xdt1[,1],xdt1[,-1]-xdt2[,-1])
hist(as.matrix(xdt1[,-1]-xdt2[,-1]), breaks=10)


xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)




xdt1 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == 0 & pvalue_method == "Monte Carlo" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
xdt2 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == 0 & pvalue_method == "bootstrap" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
cbind(xdt1[,1],xdt1[,-1]-xdt2[,-1])
hist(as.matrix(xdt1[,-1]-xdt2[,-1]), breaks=10)


xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == 0 & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)




# Sh = I -- single
xdt1 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == .2 & dtau_type == "single" & pvalue_method == "Monte Carlo" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
xdt2 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == .2 & dtau_type == "single" & pvalue_method == "bootstrap" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
cbind(xdt1[,1],xdt1[,-1]-xdt2[,-1])
hist(as.matrix(xdt1[,-1]-xdt2[,-1]), breaks=10)



xdt <- dcast(dt2[norm == "Euclidean" & S == "I" &
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "N")
xdt
print(xtable(xdt,digits=1),include.rownames = F)




xdt1 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == .2 & dtau_type == "single" & pvalue_method == "Monte Carlo" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
xdt2 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == .2 & dtau_type == "single" & pvalue_method == "bootstrap" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
cbind(xdt1[,1],xdt1[,-1]-xdt2[,-1])
hist(as.matrix(xdt1[,-1]-xdt2[,-1]), breaks=10)


xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .2 & dtau_type == "single" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)




# S = Sh -- column
xdt1 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == .1 & dtau_type == "column" & pvalue_method == "Monte Carlo" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
xdt2 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == .1 & dtau_type == "column" & pvalue_method == "bootstrap" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
cbind(xdt1[,1],xdt1[,-1]-xdt2[,-1])
hist(as.matrix(xdt1[,-1]-xdt2[,-1]), breaks=10)


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



xdt1 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == .1 & dtau_type == "column" & pvalue_method == "Monte Carlo" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
xdt2 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% c(50,100,150) & d %in% c(5,15,25) &
                    dtau == .1 & dtau_type == "column" & pvalue_method == "bootstrap" & 
                    tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
              formula = d ~ tau + n,
              value.var = "level")
cbind(xdt1[,1],xdt1[,-1]-xdt2[,-1])
cbind(xdt1[,1],xdt1[,-1]/xdt2[,-1])
hist(as.matrix(xdt1[,-1]-xdt2[,-1]), breaks=10)


xdt <- dcast(dt2[norm == "Supremum" & S == "I" & 
                   n %in% c(50,100,150) & d %in% c(5,15,25) &
                   dtau == .1 & dtau_type == "column" & 
                   tau %in% c(0,.3,.6) & Sh %in% c("ShJ")],
             formula = Sh + pvalue_method + d ~ tau + n,
             value.var = "level")
xdt
print(xtable(xdt,digits=1),include.rownames = F)

