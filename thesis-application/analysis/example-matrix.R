library(wesanderson)
source("thesis-application/setup/constructFun.R")

TauY1 <- createMatrix(2, "equi", .99)
TauY2 <- createMatrix(3, "equi", .85)
TauY3 <- createMatrix(5, "equi", .7)

TauY <- createMatrix(3, "un1", .4)
TauY[matrix(c(1,2,2,1),2,2)] <- .475
TauY <- nestMartix(TauY,list(TauY1,TauY2,TauY3))

TauZ1 <- createMatrix(5, "un1", .725)
set.seed(672)
TauZ1[t(combn(5,2))] <- TauZ1[t(combn(5,2))] + rnorm(10,0,.12)
TauZ1 <- (TauZ1 + t(TauZ1))/2
diag(TauZ1) <- 1
is.positive.definite(sin(pi*TauZ1/2))

TauZ2 <- createMatrix(3, "equi", .35)

TauZ <- createMatrix(2,"equi",.2)
TauZ <- nestMartix(TauZ,list(TauZ1,TauZ2))

Tau <- createMatrix(2,"equi",.05)
Tau <- nestMartix(Tau,list(TauY,TauZ))

