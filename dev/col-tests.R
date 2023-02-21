library(wesanderson)
library(dendextend)
source("thesis-archive/thesis-application-3/setup/constructFun.R")
source("thesis-archive/thesis-application-3/functions/initializeDend.R")

# pal <- heat.colors(100)
pal <- c(wes_palette("Zissou1", 99, type = "continuous"),"gray10")

TauY1 <- createMatrix(2, "un1", .7)
TauY2 <- createMatrix(3, "un2", .65)
TauY3 <- createMatrix(5, "un3", .55)

TauY <- createMatrix(3, "un2", .25)
TauY <- nestMartix(TauY,list(TauY1,TauY2,TauY3))
image(t(TauY[ncol(TauY):1,]), col = pal, zlim = c(0,1), axes = F)

Tau <- TauY
X <- mvtnorm::rmvnorm(n=250, sigma = sin(pi*Tau/2))
image(t(pcaPP::cor.fk(X)[ncol(X):1,]), col = pal, zlim = c(0,1), axes = F)


####
n <- nrow(X)
d <- ncol(X)
conc <- tautests::conc(X)
Tau.hat <- apply(conc,c(2,3),sum)/choose(n,2) - 1
dend <- initializeDend(Tau.hat, "mcquitty")
dimnames(conc) <- list(1:n, 1:d, 1:d)
####

pvals <- sapply(as.character(1:d), function(i){
  node <- dend
  G <- NULL
  while(length(node) > 1){
    Gs <- lapply(node, get_leaves_attr, attribute="label")
    k <- which(sapply(Gs, function(G) i %in% G))
    G <- c(Gs[-k],G)
    node <- node[[k]]
  }
  
  S <- 16*cov(conc[,i,unlist(G)])/(n*(n-1))
  Si <- solve(S)
  th <- colSums(conc[,i,unlist(G)])/choose(n,2) - 1
  B <- sapply(G, function(g) as.integer(unlist(G) %in% g))
  tt <- c(B %*% solve(t(B) %*% S %*% B) %*% t(B) %*% S %*% th)
  
  loss <- n*mahalanobis(th, tt, Si, T)
  pval <- pchisq(loss, df = length(th)-length(G), lower.tail = F)
  pval
})

plot(pvals, ylim=c(0,1))
#


plot(dend)
dend[[c(3)]] <- unbranch(dend[[c(3)]], 3)
plot(dend)





