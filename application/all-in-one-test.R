library("data.table")


# turn quotes into weekly returns ----------------------------------------

quotes <- fread("application/quotes_all.csv")
returns <- as.matrix(apply(as.matrix(quotes[,-1]), 2, function(v) log(v[-1]/v[-length(v)])))
rownames(returns) <- quotes$date[-1]

# wednesday is "less missing"... 
table(wday(rownames(returns))) 

# in fact there is not much missing
difftime(max(quotes$date),min(quotes$date),units = "weeks")

# so let us pick every Wednesday
returns <- returns[wday(rownames(returns)) == 4,]

#saveRDS(returns, "application/returns_mat.rds")





library(data.table)
library(pcaPP)
library(Matrix)
library(mvtnorm)

source("sim-main/functions/computeTh.R")
source("sim-main/functions/buildSigma.R")
source("sim-main/functions/testEquiRankJackknife.R")
source("sim-main/functions/testEquiRankPlugin.R")

# X <- returns[1:100,]
X <- returns
meta <- fread("application/NASDAQ100_meta.csv")


table(meta[,Sector])
sectors <- meta[,unique(Sector)]

par(mfrow=c(4,1), mar = c(2,2,1,1))
sapply(seq(1,ncol(X),length.out = 4), function(k){
  plot(X[,k])
  lines(X[,k], col="red")
})
par(mfrow=c(1,1))


# sector equi-correlation -------------------------------------------------

par(mfrow=c(2,4))
pvals <- sapply(sectors[-1], function(sec){
  print(sec)
  nn <- meta[Sector %in% sec, unlist(Symbol)]
  Th <- cor.fk(X[,nn])
  image(t(Th[nrow(Th):1,]), col = rainbow(100), zlim = c(-1,1))
  if(length(nn) <= 2) return(rep(1,5))
  
  # pval1 <- testEquiRankPlugin(X[,nn])
  pval2 <- testEquiRankJackknife(X[,nn], M = 2000)
  
  # print(pval1)
  pval2
})

pvals

library(xtable)
xtable(round(do.call("rbind",pvals[-length(pvals)]),3), digits = 3)

# some sectors clearly fail the test
# except for Technology, we should trust only the classical tests
# let us focus on the technology sector



# techonology intra-sector structure --------------------------------------

# sec <- "Techonology"
sec <- sectors[1]

nn <- meta[Sector %in% sec, unlist(Symbol)]
Th <- cor.fk(X[,nn])
par(mfrow=c(1,1))
image(t(Th[nrow(Th):1,]), col = rainbow(100), zlim = c(-1,1))

# GOOG and GOOGL are way too correlated
# Suppose that we keep only one of them
nn <- nn[-which(nn == "GOOGL")]
Th <- cor.fk(X[,nn])
par(mfrow=c(1,1))
image(t(Th[nrow(Th):1,]), col = rainbow(100), zlim = c(-1,1))

tER.low <- testEquiRank(X[,nn])
tER.high <- testEquiRankMC(X[,nn], M = 500)

pval <- c(tER.low$pval[c(1,4,7,10)],tER.high)
names(pval) <- c(tER.low$test_type[c(1,4,7,10)],"MC")
pval

# Still very bad, let us cut above tau=.5
ind <- which(sapply(1:nrow(Th), function(i) max(Th[i,-i])) > .5)
nn <- nn[-ind]
Th <- cor.fk(X[,nn])
par(mfrow=c(1,1))
image(t(Th[nrow(Th):1,]), col = rainbow(100), zlim = c(-1,1))

tER.low <- testEquiRank(X[,nn])
tER.high <- testEquiRankMC(X[,nn], M = 500)

pval <- c(tER.low$pval[c(1,4,7,10)],tER.high)
names(pval) <- c(tER.low$test_type[c(1,4,7,10)],"MC")
pval





D <- dist(sqrt(1-abs(Th)))
# D <- as.matrix(D)
# D["AMAT","ASML"] <- 100
# D["ASML","AMAT"] <- 100
# D <- as.dist(D)
hc <- hclust(D)
# hc <- hclust(dist(Th-mean(Th[ij.mat])))
# hc <- hclust(dist(Th-mean(Th[ij.mat])), "complete")
plot(hc)
oo <- hc$order
image(t(Th[rev(oo),oo]), col = rainbow(100), zlim = c(-1,1))
# 
# 
# K <- 10
# clus <- cutree(hc,K)
# table(clus)
# 
# 
# rbindlist(lapply(1:K, function(k){
#   ind <- which(clus==k)
#   image(t(Th[rev(ind),ind]), col = rainbow(100), zlim = c(-1,1))
#   if(length(ind) > 2) testEquiRank(X[,nn[ind]])[c(1),"pval"]
#   else data.table(1)
# }))

clus.mat <- sapply(25:37, function(k) cutree(hc,k))

pvals <- apply(clus.mat, 2, function(clus){
  print(clus)
  testEquiRankChen(X=X, clus=clus, 1000)
})
pvals

plot(pvals)

lapply(1:(ncol(clus.mat)-1), function(k){
  K1 <- length(unique(clus.mat[,k]))
  K2 <- length(unique(clus.mat[,k+1]))
  
  B1 <- matrix(0,d,K1)
  B2 <- matrix(0,d,K2)
  
  B1[cbind(1:d,clus.mat[,k])] <- 1
  B2[cbind(1:d,clus.mat[,k+1])] <- 1
  
  D1 <- tcrossprod(B1)
  D2 <- tcrossprod(B2)
  
  abs(D1 - D2)
})

order(diff(pvals))

clus.mat[,6]