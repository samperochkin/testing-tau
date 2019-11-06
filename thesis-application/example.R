library(wesanderson)
source("thesis-application/setup/constructFun.R")

# pal <- heat.colors(100)
pal <- wes_palette("Zissou1", 120, type = "continuous")
cols <- c(rep("white",90),pal,rep("black",100))

TauY1 <- createMatrix(2, "equi", .95)
TauY2 <- createMatrix(3, "equi", .85)
TauY3 <- createMatrix(5, "equi", .7)

diag(TauY1) <- diag(TauY2) <- diag(TauY3) <- 2

pdf(width = 1, height = 1, file = "TauY1.pdf")
par(mar=c(0,0,0,0))
image(t(TauY1[ncol(TauY1):1,]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

pdf(width = 1.5, height = 1.5, file = "TauY2.pdf")
par(mar=c(0,0,0,0))
image(t(TauY2[ncol(TauY2):1,]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

pdf(width = 2.5, height = 2.5, file = "TauY3.pdf")
par(mar=c(0,0,0,0))
image(t(TauY3[ncol(TauY3):1,]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

TauY <- createMatrix(3, "un1", .4)
TauY[matrix(c(1,2,2,1),2,2)] <- .475
diag(TauY) <- 2

pdf(width = 2.5, height = 2.5, file = "TauY-1.pdf")
par(mar=c(0,0,0,0))
image(t(TauY[ncol(TauY):1,]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

TauY <- nestMartix(TauY,list(TauY1,TauY2,TauY3))
pdf(width = 5, height = 5, file = "TauY-2.pdf")
par(mar=c(0,0,0,0))
image(t(TauY[ncol(TauY):1,]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

TauZ1 <- createMatrix(5, "un1", .8)
set.seed(672)
TauZ1[t(combn(5,2))] <- TauZ1[t(combn(5,2))] + rnorm(10,0,.05)
TauZ1 <- (TauZ1 + t(TauZ1))/2
diag(TauZ1) <- 1
is.positive.definite(sin(pi*TauZ1/2))

TauZ2 <- createMatrix(3, "equi", .375)

diag(TauZ1) <- diag(TauZ2) <- 2

TauZ <- createMatrix(2,"equi",.2)
TauZ <- nestMartix(TauZ,list(TauZ1,TauZ2))
is.positive.definite(sin(pi*TauZ/2))

Tau <- createMatrix(2,"equi",.05)
Tau <- nestMartix(Tau,list(TauY,TauZ))


pdf(width = 5, height = 5, file = "Tau.pdf")
par(mar=c(0,0,0,0))
image(t(Tau[ncol(Tau):1,]), col = cols, zlim = c(-1,2), axes = F)
dev.off()



####

