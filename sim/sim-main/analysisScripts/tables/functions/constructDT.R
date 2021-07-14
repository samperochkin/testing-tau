contructDT <- function(fii){
  
  dt <- data.table()
  for(dd in c("t4", "hac")){
    ii <- grep(dd, fii)
    dt0 <- rbindlist(lapply(fii[ii], fread), fill=T)
    
    dt0$dtau_type <- factor(dt0$dtau_type, level = c("none","single","column"))
    dt0[,decision := pvalue < .05]
    
    dt0$dtau_type <- factor(dt0$dtau_type,levels = c("none","single","column"))
    dt0$S <- factor(dt0$S,levels = c("Sh","I","Sh-p","Sh-d"))
    dt0$Sh <- factor(dt0$Sh,levels = c("ShP","ShJ","SbP","SbJ"))
    
    dt0 <- dt0[S %in% c("Sh","I")]
    dt0 <- dt0[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)),
          N = .N, mean_PSD = mean(isShPsd)),
       by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","distribution")]
    
    dt <- rbind(dt,dt0)
  }
  
  dt
}