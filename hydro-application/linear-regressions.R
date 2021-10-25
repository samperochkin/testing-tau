par(mfrow = c(1,4), mar=c(2,2,2,1))
sapply(1:18, function(i){
  id <- colnames(X)[i]
  nam <- stns %>% subset(ID == as.integer(id)) %>% pull(`Station Name`)
  cou <- stns %>% subset(ID == as.integer(id)) %>% pull(Country)
  y <- X[,i]
  x <- data2$YEAR - min(data2$YEAR)
  su <- summary(lm(y~x))
  plot(x,y,main=paste0(nam, ", ", cou, " (",i,")"))
  lines(x,y)
  lines(x,su$coefficients[1] + x*su$coefficients[2], col="blue")
})
