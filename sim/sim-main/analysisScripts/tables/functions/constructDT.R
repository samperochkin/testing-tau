contructDT <- function(fii, dis=NULL){
  
  if(is.null(dis)) dis <- c("normal", "t4", "hac", "clayton", "gumbel")
  dis <- paste0("_", dis)
  
  iis <- lapply(dis, function(dd) grep(dd,fii))
  iis <- iis[sapply(iis, length) > 0] 
  
  dt <- data.table()
  for(ii in iis){
    dt0 <- rbindlist(lapply(fii[ii], fread), fill=T)
    
    dt0$dtau_type <- factor(dt0$dtau_type, level = c("none","single","column"))
    dt0[,decision := pvalue < .05]
    
    dt0$dtau_type <- factor(dt0$dtau_type,levels = c("none","single","column"))
    dt0$S <- factor(dt0$S,levels = c("Sh","I","Sh-p","Sh-d"))
    dt0$Sh <- factor(dt0$Sh,levels = c("ShP","ShJ","SbP","SbJ"))
    
    dt0 <- dt0[S %in% c("Sh","I")]
    dt0 <- dt0[,.("rejection_rate" = 100*mean(decision,na.rm=T), "pvalue_NA_rate" = mean(is.na(pvalue)),
          N = .N, psd_rate = mean(isShPsd), pd_rate = mean(isShPd)),
       by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","distribution")]
    
    dt <- rbind(dt,dt0)
  }
  
  dt
}