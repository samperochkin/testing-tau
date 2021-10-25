load("application/quotes.Rdata")
meta <- fread("application/NASDAQ100_meta.csv")
range(index(NASDAQ100_quotes$AAL))

meta[Symbol %in% c("TSLA","EA","NVDA","AMZN","MNST","AMGN")]

X <- readRDS("application/returns_mat.rds")
range(rownames(X))
index(NASDAQ100_quotes$AAL)[which(index(NASDAQ100_quotes$AAL) %in% rownames(X))]

ind <- index(NASDAQ100_quotes$AAL)[which(index(NASDAQ100_quotes$AAL) %in% rownames(X))]
#ind <- tail(ind,n)
rm(NASDAQ100_quotes)

colnames(X)
par(mfrow=c(5,1), mar = c(2,2,1.5,1))
ran <- range(c(X[,"TSLA"],X[,"EA"],X[,"NVDA"],X[,"MNST"])) + c(-.01,.01)
plot(ind ,X[,"TSLA"], type="l", ylab="", ylim=ran, xaxt="n", yaxt="n")
axis(2,at=c(0))
axis(2,at=c(-.2,.2))
title(main=expression(r[t]~~(Tesla~-~TSLA)), line=.75)
#
par(mar = c(2,2,1.5,1))
plot(ind, X[,"EA"], type="l", ylim=ran, yaxt="n", xaxt="n")
axis(2,at=c(0))
axis(2,at=c(-.2,.2))
title(main=expression(r[t]~~(Electronic~Arts~-~EA)), line=.75)
#
par(mar = c(2,2,1.5,1))
plot(ind, X[,"NVDA"], type="l", ylim=ran, yaxt="n", xaxt="n")
axis(2,at=c(0))
axis(2,at=c(-.2,.2))
title(main=expression(r[t]~~(NVIDIA~-~NVDA)), line=.75)
#
par(mar = c(2,2,1.5,1))
plot(ind, X[,"MNST"], type="l", ylim=ran, yaxt="n", xaxt="n")
axis(2,at=c(0))
axis(2,at=c(-.2,.2))
title(main=expression(r[t]~~(Monster~Beverage~-~MNST)), line=.75)
#
par(mar = c(2,2,1.5,1))
plot(ind, X[,"AMGN"], type="l", ylim=ran, yaxt="n")
axis(2,at=c(0))
axis(2,at=c(-.2,.2))
title(main=expression(r[t]~~(Amgen~-~AMGN)), line=.75)



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
