# Watch out -- results for S=I and Sh=ShJ are pooled (MC and bootstrap)
library(data.table)
library(ggplot2)
library(xtable)

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("sim/sim-main/results/ulaval-server-results",full.names = T, pattern = ".csv")
fii <- fii[grepl("low", fii)]
fii <- fii[!grepl("block", fii)]
dt <- rbindlist(lapply(fii,fread), fill=T)
all(dt$isShPd)

# dt[, s_info := NULL]

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

#### À voir
# dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)), "NN" = .N,
             mean_PD = mean(isShPd), mean_PsD = mean(isShPsd)),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","distribution")]

dt2[S == "Sh" & d == 5 & Sh == "ShP" & dtau == 0 & norm == "Euclidean"]
dt2[S == "Sh" & d == 15 & Sh == "ShP" & dtau == 0 & n %in% c(250)]
dt2[S == "Sh" & d == 15 & Sh == "ShP" & dtau == 0 & n %in% c(150)]

