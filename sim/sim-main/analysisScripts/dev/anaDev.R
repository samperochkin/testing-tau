library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)
#fii <- fii[grepl("low",fii)]

dt <- rbindlist(lapply(fii[1:3],fread))
dt[, s_info := NULL]
dt <- rbindlist(c(list(dt),lapply(fii[6:7],fread)))

dt$isShPsd <- NA
dt$rankSh <- NA
dt <- rbindlist(c(list(dt),lapply(fii[c(4,5)],fread)))

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))

ggplot(dt[n == 150 & d == 5 &
            tau == 0.3 & dtau %in% c(0,.1) &
            S == "I" & pvalue_method != "bootstrap"],
       aes(x = pvalue, fill = isShPd)) + 
  geom_histogram(breaks = seq(0,1,.1), position = "stack",
                 aes(y=..count../sum(..count..))) +
  facet_grid(dtau_type+dtau+norm~S+Sh, scales="free")


ggplot(dt[n == 150 & d == 5 &
            tau == 0.3 & dtau %in% c(0,.1) &
            S == "Sh" & pvalue_method != "bootstrap"],
       aes(x = pvalue, fill = isShPd)) + 
  geom_histogram(breaks = seq(0,1,.1),
                 aes(y=..count../sum(..count..))) +
  facet_grid(dtau_type+dtau+norm~S+Sh, scales="free")


#
ggplot(dt[n == 150 & Sh == "ShJ" &
            dtau == 0 & S == "Sh"],
       aes(x = pvalue, fill = isShPd)) + 
  geom_histogram(breaks = seq(0,1,.1),
                 aes(y=..count../sum(..count..))) +
  facet_grid(norm~d, scales="free")


dt[n == 150 & Sh == "ShJ" &
     dtau == 0 & S == "Sh", .(N = .N),.(isShPd,norm,d,tau)]


dt[,decision := pvalue < .05]
dt2 <- dt[,.("level" = mean(decision,na.rm=T)), by=c("n","d","tau",
                                                     "S","Sh","norm","dtau","dtau_type","pvalue_method",
                                                     "isShPd")]
ggplot(dt2[n == 150 & d == 5 & tau == .3 & dtau %in% c(0,.1)], aes(x=S, y=level, fill=Sh)) +
  geom_bar(stat="identity",position=position_dodge2()) +
  geom_abline(intercept=.05, slope=0, lty=2, col="blue") +
  facet_grid(dtau+dtau_type~norm) + ylim(0,1)


dt3 <- dt2[dtau_type == "none",]
dt3[,dtau_type := "single"]
dt3 <- rbindlist(list(dt2,dt3))
dt3[dtau_type == "none", dtau_type := "column"]
ggplot(dt3[n == 150 & d == 5 & tau == 0.3], aes(x=dtau, y=level, col=Sh, linetype=S)) +
  geom_line() +
  facet_grid(tau+dtau_type~norm) + ylim(0,1)


dt2[norm == "Euclidean", norm := "E"]
dt2[norm == "Supremum", norm := "M"]
dt2[,level := level*100]

xdt <- dcast(dt2[isShPd %in% c(T,NA) &
                   dtau == 0 &
                   tau %in% c(0,.3) &
                   Sh %in% c("ShP","ShJ") &
                   n %in% c(50,150),], tau + n + d + pvalue_method ~ norm + S + Sh, value.var = "level")
# xdt <- subset(xdt,select=!apply(is.na(xdt),2,any))
print(xtable(xdt,digits=1),include.rownames = F)




dt4 <- dt[,.("level" = mean(decision,na.rm=T)), by=c("n","d","tau",
                                                     "S","Sh","norm","dtau","dtau_type","pvalue_method")]

dt4[norm == "Euclidean", norm := "E"]
dt4[norm == "Supremum", norm := "M"]
dt4[,level := level*100]

dt4[grepl("J",Sh), Sh_type1 := "Jackknife"]
dt4[grepl("P",Sh), Sh_type1 := "Plugin"]

dt4[grepl("h",Sh), Sh_type2 := "h"]
dt4[grepl("b",Sh), Sh_type2 := "b"]

dt4$Sh_type2 <- factor(dt4$Sh_type2, levels = c("h","b"))

xdt <- dcast(dt4[  n %in% c(50,150) & d %in% c(5,25) &
                   dtau == 0 &
                   tau %in% c(0,.3),],  Sh_type2 + tau + n + d + pvalue_method ~ norm + S + Sh_type1, value.var = "level")
# xdt <- subset(xdt,select=!apply(is.na(xdt),2,any))
print(xtable(xdt,digits=1),include.rownames = F)



