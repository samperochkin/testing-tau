load("application/quotes.Rdata")

n <- nrow(X)
ind <- index(NASDAQ100_quotes$AAL)[which(index(NASDAQ100_quotes$AAL) %in% rownames(X))]
ind <- tail(ind,n)

par(mfrow=c(2,1), mar = c(2,2,1.5,1))
ran <- range(c(X[,"TSLA"],X[,"EA"])) + c(-.01,.01)
plot(ind ,X[,"TSLA"], type="l", ylab="", ylim=ran, xaxt="n", yaxt="n")
axis(2,at=c(0))
axis(2,at=c(-.2,0,.2))
title(main=expression(r[t]~~(Tesla~-~TSLA)), line=.75)
par(mar = c(2,2,1.5,1))
plot(ind ,X[,"EA"], type="l", ylim=ran, yaxt="n")
axis(2,at=c(0))
axis(2,at=c(-.2,0,.2))
title(main=expression(r[t]~~(Electronic~Arts~-~EA)), line=.75)

rm(NASDAQ100_quotes)
