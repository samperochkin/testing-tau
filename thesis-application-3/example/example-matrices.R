library(wesanderson)
source("thesis-application/setup/constructFun.R")

# pal <- heat.colors(100)
pal <- wes_palette("Zissou1", 112, type = "continuous")
cols <- c(rep("white",88),pal,rep("black",100))

TauY1 <- createMatrix(2, "equi", .95)
TauY2 <- createMatrix(3, "equi", .85)
TauY3 <- createMatrix(5, "equi", .55)

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

TauY <- createMatrix(3, "equi", .125)
TauY[matrix(c(1,2,2,1),2,2)] <- .375
TauY[matrix(c(2,3,3,2),2,2)] <- .25
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

TauZ2 <- createMatrix(3, "equi", .325)

diag(TauZ1) <- diag(TauZ2) <- 2


Tau <- createMatrix(3,"equi",0)
Tau <- nestMartix(Tau,list(TauY,TauZ1,TauZ2))


pdf(width = 5, height = 5, file = "Tau.pdf")
par(mar=c(0,0,0,0))
image(t(Tau[ncol(Tau):1,]), col = cols, zlim = c(-1,2), axes = F)
dev.off()


pdf(width = 5, height = 5, file = "Tau-annot1.pdf")
par(mar=c(0,0,0,0))
image(t(Tau[ncol(Tau):1,]), col = cols, zlim = c(-1,2), axes = F)

text(x = 3/18, y = 17.5/18, labels = expression(bold(T)["(0,1)"]^"(1,2)"), cex = 1.5)
text(x = 7.25/18, y = 17.5/18, labels = expression(bold(T)["(0,1)"]^"(1,3)"), cex = 1.5)
text(x = 7.25/18, y = 14.75/18, labels = expression(bold(T)["(0,1)"]^"(2,3)"), cex = 1.5)
dev.off()


pdf(width = 5, height = 5, file = "Tau-annot2.pdf")
par(mar=c(0,0,0,0))
image(t(Tau[ncol(Tau):1,]), col = cols, zlim = c(-1,2), axes = F)

abline(h=7.97/18, lty = 2)
abline(h=2.7/18, lty = 2)
abline(v=10.05/18, lty = 2)
abline(v=15.35/18, lty = 2)

text(x = 13/18, y = 13.5/18, labels = expression(bold(T)["(0)"]^"(1,2)"), cex = 1.9)
text(x = 17.1/18, y = 13.5/18, labels = expression(bold(T)["(0)"]^"(1,3)"), cex = 1.9)
text(x = 17.1/18, y = 5.1/18, labels = expression(bold(T)["(0)"]^"(2,3)"), cex = 1.9)
dev.off()


pdf(width = 5, height = 5, file = "Tau-annot3.pdf")
par(mar=c(0,0,0,0))
image(t(Tau[ncol(Tau):1,]), col = cols, zlim = c(-1,2), axes = F)
# text(x = 14.5/18, y = 12.5/18, labels = expression("G"["(0)"]^"(1,2)"*" U "*"G"["(0)"]^"(1,3)"*" U "*"G"["(0)"]^"(2,3)"), cex = 1.25)
# text(x = 15/18, y = 12.5/18, labels = expression(bold(T)["(0)"]^(""["*"]*","["*"])), cex = 3)
text(x = 15/18, y = 12.5/18, labels = expression(bold(T)["(0)"]^(paste("", "", symbol("·"), ""))), cex = 3)
dev.off()

# library(latex2exp)
# TeX('$\\bullet$')
# expression(`$\bullet$` = paste("", "", symbol("·"), ""))

####
