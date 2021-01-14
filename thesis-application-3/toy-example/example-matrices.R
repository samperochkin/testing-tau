library(wesanderson)
source("thesis-application/setup/constructFun.R")

# pal <- heat.colors(100)
pal <- wes_palette("Zissou1", 116, type = "continuous")
cols <- c(rep("white",84),pal,rep("black",100))

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


# pdf(width = 5, height = 5, file = "Tau-annot1.pdf")
# par(mar=c(0,0,0,0))
# image(t(Tau[ncol(Tau):1,]), col = cols, zlim = c(-1,2), axes = F)
# 
# text(x = 3/18, y = 17.5/18, labels = expression(bold(T)["(0,1)"]^"(1,2)"), cex = 1.5)
# text(x = 7.25/18, y = 17.5/18, labels = expression(bold(T)["(0,1)"]^"(1,3)"), cex = 1.5)
# text(x = 7.25/18, y = 14.75/18, labels = expression(bold(T)["(0,1)"]^"(2,3)"), cex = 1.5)
# dev.off()
# 
# 
# pdf(width = 5, height = 5, file = "Tau-annot2.pdf")
# par(mar=c(0,0,0,0))
# image(t(Tau[ncol(Tau):1,]), col = cols, zlim = c(-1,2), axes = F)
# 
# abline(h=7.97/18, lty = 2)
# abline(h=2.7/18, lty = 2)
# abline(v=10.05/18, lty = 2)
# abline(v=15.35/18, lty = 2)
# 
# text(x = 13/18, y = 13.5/18, labels = expression(bold(T)["(0)"]^"(1,2)"), cex = 1.9)
# text(x = 17.1/18, y = 13.5/18, labels = expression(bold(T)["(0)"]^"(1,3)"), cex = 1.9)
# text(x = 17.1/18, y = 5.1/18, labels = expression(bold(T)["(0)"]^"(2,3)"), cex = 1.9)
# dev.off()
# 
# 
# pdf(width = 5, height = 5, file = "Tau-annot3.pdf")
# par(mar=c(0,0,0,0))
# image(t(Tau[ncol(Tau):1,]), col = cols, zlim = c(-1,2), axes = F)
# # text(x = 14.5/18, y = 12.5/18, labels = expression("G"["(0)"]^"(1,2)"*" U "*"G"["(0)"]^"(1,3)"*" U "*"G"["(0)"]^"(2,3)"), cex = 1.25)
# # text(x = 15/18, y = 12.5/18, labels = expression(bold(T)["(0)"]^(""["*"]*","["*"])), cex = 3)
# text(x = 15/18, y = 12.5/18, labels = expression(bold(T)["(0)"]^(paste("", "", symbol("·"), ""))), cex = 3)
# dev.off()
# 
# # library(latex2exp)
# # TeX('$\\bullet$')
# # expression(`$\bullet$` = paste("", "", symbol("·"), ""))
# 
####



d <- 18
eps <- 1/(d-1)
ss <- seq(-eps/2,1+eps/2,length.out=d+1)
diag(Tau) <- 2

pdf(width = 5, height = 5, file = "Tau-annot.pdf")

  par(mar=c(0,0,0,0))
  image(t(Tau[d:1,]), col = cols, zlim = c(-1,2), axes = F)

  blocks <- matrix(c(3,17,6,19,
                   6,17,11,19,
                   6,14,11,17),4,3)
  # apply(blocks, 2, function(bb){
  #   xleft, ybottom, xright, ytop
  #   rect(ss[bb[1]],ss[bb[2]],ss[bb[3]],ss[bb[4]],lty=1)
  # })
  bb <- blocks[,1]
  text(x = (ss[bb[1]]+ss[bb[3]])/2, y = (ss[bb[2]]+ss[bb[4]])/2, labels = expression(bold(T)["(0,1)"]^"(1,2)"), cex = 1.45)
  bb <- blocks[,2]
  text(x = (ss[bb[1]]+ss[bb[3]])/2, y = (ss[bb[2]]+ss[bb[4]])/2, labels = expression(bold(T)["(0,1)"]^"(1,3)"), cex = 1.45)
  bb <- blocks[,3]
  text(x = (ss[bb[1]]+ss[bb[3]])/2, y = (ss[bb[2]]+ss[bb[4]])/2, labels = expression(bold(T)["(0,1)"]^"(2,3)"), cex = 1.45)

  blocks <- matrix(c(11,9,16,19,
                     16,9,19,19,
                     16,4,19,9),4,3)
  
  apply(blocks, 2, function(bb){
    # xleft, ybottom, xright, ytop
    rect(ss[bb[1]],ss[bb[2]],ss[bb[3]],ss[bb[4]],lty=1)
  })
  
  bb <- blocks[,1]
  text(x = (ss[bb[1]]+ss[bb[3]])/2, y = (ss[bb[2]]+ss[bb[4]])/2, labels = expression(bold(T)["(0)"]^"(1,2)"), cex = 1.9)
  bb <- blocks[,2]
  text(x = (ss[bb[1]]+ss[bb[3]])/2, y = (ss[bb[2]]+ss[bb[4]])/2, labels = expression(bold(T)["(0)"]^"(1,3)"), cex = 1.9)
  bb <- blocks[,3]
  text(x = (ss[bb[1]]+ss[bb[3]])/2, y = (ss[bb[2]]+ss[bb[4]])/2, labels = expression(bold(T)["(0)"]^"(2,3)"), cex = 1.9)
dev.off()




source("thesis-application-3/example/example-empirical-2.R")
Tau.hat <- cor.fk(X)
diag(Tau.hat) <- 2

pdf(width = 5, height = 5, file = "Th-annot.pdf")

  par(mar=c(0,0,0,0))
  image(t(Tau.hat[d:1,]), col = cols, zlim = c(-1,2), axes = F)

  blocks <- matrix(c(11,9,16,19,
                   16,9,19,19,
                   16,4,19,9),4,3)

  apply(blocks, 2, function(bb){
    # xleft, ybottom, xright, ytop
    rect(ss[bb[1]],ss[bb[2]],ss[bb[3]],ss[bb[4]],lty=1)
  })

  segments(x0=ss[11] , y0=ss[17], x1=ss[19] , y1=ss[17], lty=2)
  segments(x0=ss[11] , y0=ss[14], x1=ss[19] , y1=ss[14], lty=2)
  
  text(x = (ss[11]+ss[16])/2, y = (ss[17]+ss[19])/2, labels = expression(bold(A)["1"]), cex = 1.6)
  text(x = (ss[11]+ss[16])/2, y = (ss[14]+ss[17])/2, labels = expression(bold(A)["2"]), cex = 1.6)
  text(x = (ss[11]+ss[16])/2, y = (ss[9]+ss[14])/2, labels = expression(bold(A)["3"]), cex = 1.6)
  
dev.off()



Th2 <- diag(d-10+3)*2
Th2[4:11,4:11] <- Tau.hat[11:18,11:18]

Gs <- c(list(1:2,3:5,6:10),as.list(11:18))
K <- length(Gs)
eps <- 1/(K-1)
ss <- seq(-eps/2,1+eps/2,length.out=K+1)

for(k in 1:3){
  Th2[k,(1:K)[-k]] <- Th2[(1:K)[-k],k] <- sapply(Gs[-k], function(G){mean(Tau.hat[Gs[[k]],G])})
}

pdf(width = 5*K/d, height = 5*K/d, file = "Th-interim.pdf")

  par(mar=c(0,0,0,0))
  image(t(Th2[K:1,]), col = cols, zlim = c(-1,2), axes = F)

  blocks <- matrix(c(4,9,9,12,
                    9,9,12,12,
                    9,4,12,9),4,3)

  apply(blocks, 2, function(bb){
    # xleft, ybottom, xright, ytop
    rect(ss[bb[1]],ss[bb[2]],ss[bb[3]],ss[bb[4]],lty=1)
  })
  segments(x0=ss[4] , y0=ss[11], x1=ss[12] , y1=ss[11], lty=2)
  segments(x0=ss[4] , y0=ss[10], x1=ss[12] , y1=ss[10], lty=2)
  
dev.off()





Th3 <- diag(3)*2

Gs <- list(1:3,4:8,9:11)
K <- length(Gs)
eps <- 1/(K-1)
ss <- seq(-eps/2,1+eps/2,length.out=K+1)

for(k in 1:3){
  Th3[k,(1:K)[-k]] <- Th3[(1:K)[-k],k] <- sapply(Gs[-k], function(G){mean(Th2[Gs[[k]],G])})
}

pdf(width = 5*K/d, height = 5*K/d, file = "Th-dag.pdf")

  par(mar=c(0,0,0,0))
  image(t(Th3[K:1,]), col = cols, zlim = c(-1,2), axes = F)

  blocks <- matrix(c(2,3,3,4,
                     3,3,4,4,
                     3,2,4,3),4,3)

  apply(blocks, 2, function(bb){
    # xleft, ybottom, xright, ytop
    rect(ss[bb[1]],ss[bb[2]],ss[bb[3]],ss[bb[4]],lty=1)
  })

dev.off()
