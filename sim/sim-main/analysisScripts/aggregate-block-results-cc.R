library(data.table)

fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
fii <- fii[grepl("block", fii)]
dt <- fread(fii)

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single"))
dt[,decision := pvalue < .05]

dt$S <- factor(dt$S,levels = c("Sh","I"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

dt <- dt[S %in% c("Sh","I")]
dt <- dt[,.("rejection_rate" = 100*mean(decision,na.rm=T), "pvalue_NA_rate" = mean(is.na(pvalue)),
            N = .N, psd_rate = mean(isShPsd), pd_rate = mean(isShPd)),
         by=c("n","d","S","Sh","norm","dtau","dtau_type","distribution","design")]

print(object.size(dt), units = "MB")
fwrite(dt, "sim/sim-main/results/results-extra/dt_block_cc.csv")
