apply(Y,2,acf)
apply(Y,2,acf,type="partial")

par(mfrow=c(3,3))
acfs <- apply(Y,2,acf)
a1 <- acfs[[1]]
lapply(acfs,function(ac) ac)


summary(a1)
acf(Y[,1],)



Box.pvals <- apply(Y, 2,function(y) Box.test(y, lag = 1, type = c("Ljung-Box"), fitdf = 0)$p.value)
nn <- sapply(as.numeric(colnames(Y)), function(nn) stns[stns$ID == nn,]$`Station Name`)
names(Box.pvals) <- sapply(1:18, function(k) paste0(nn[k]," [",k,"]"))
Box.pvals


P <- sapply(1:10, function(ll){
  Box.pvals <- apply(Y, 2,function(y) Box.test(y, lag = ll, type = c("Ljung-Box"), fitdf = pmax(0,0))$p.value)
  nn <- sapply(as.numeric(colnames(Y)), function(nn) stns[stns$ID == nn,]$`Station Name`)
  names(Box.pvals) <- sapply(1:18, function(k) paste0(nn[k]," [",k,"]"))
  Box.pvals
})


apply(P < .05, 2, sum)
apply(P < .05/18, 2, sum) # bonferroni

# there could be some El Nino effect,
# that there is a lot of dependence among the time series.
# Note : Fitting a model on residuals of arima models give the same result in the end.


