# Run example-empirical-2.R before.

source("thesis-application-3/functions/validify.R")

k <- 31
dend <- struc$dend.begin.list[[k]]
v <- struc$v.list[[k]]
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
ggsave(filename = paste0("ex_arbre_",k,"_pre.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)





# #### First option
# 
# k <- 34
# dend <- struc$dend.begin.list[[k]]
# v <- struc$v.list[[k]]
# attr(dend, "delta") <- 1
# attr(dend[[v[-1]]], "delta") <- 1
# source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
# gg
# ggsave(filename = paste0("ex_arbre_",k,".pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# Thk <- struc$Tau.list[[k]]
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,".pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()
# 
# attr(dend[[v[-1]]], "delta") <- -4
# source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
# gg
# ggsave(filename = paste0("ex_arbre_",k,".pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# Thk <- struc$Tau.list[[k]]
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,".pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()
# 
# 
# dend <- struc$dend
# dend <- validify(dend)
# attr(dend,"delta") <- 1
# source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
# gg
# ggsave(filename = paste0("ex_arbre_",k,"-star0.pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# attr(dend[[v[-1]]],"delta") <- 1
# Thk <- constructTauTilde(dend,Tau.hat)
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,"-star0.pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()
# 
# 
# attr(dend, "delta") <- 0
# source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
# gg
# ggsave(filename = paste0("ex_arbre_",k,"-star0.pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# attr(dend[[v[-1]]],"delta") <- 1
# Thk <- constructTauTilde(dend,Tau.hat)
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,"-star0.pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()
# 
# 
# 
# #### Second option
# 
# 
# k <- 29
# dend <- struc$dend.begin.list[[k]]
# v <- struc$v.list[[k]]
# attr(dend[[v[-1]]], "delta") <- -3
# source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
# gg
# ggsave(filename = paste0("ex_arbre_",k,".pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# Thk <- struc$Tau.list[[k]]
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,".pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()
# 
# attr(dend[[v[-1]]], "delta") <- 1
# attr(dend[[c(v[-1],1)]], "delta") <- -4
# source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
# gg
# ggsave(filename = paste0("ex_arbre_",k,".pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# Thk <- struc$Tau.list[[k]]
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,".pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()
# 
# 
# k <- 30
# dend <- struc$dend.begin.list[[k]]
# dend <- validify(dend)
# source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
# gg
# ggsave(filename = paste0("ex_arbre_",k,"-star0.pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# attr(dend[[v[-1]]],"delta") <- 1
# Thk <- constructTauTilde(dend,Tau.hat)
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,"-star0.pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()
# 
# 
# attr(dend[[3]], "delta") <- 0
# source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
# gg
# ggsave(filename = paste0("ex_arbre_",k,"-star0.pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# attr(dend[[v[-1]]],"delta") <- 1
# Thk <- constructTauTilde(dend,Tau.hat)
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,"-star0.pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()




#### Third option


k <- 32
dend <- struc$dend.begin.list[[k]]
v <- struc$v.list[[k]]
attr(dend[[v[-1]]], "delta") <- -3
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
# ggsave(filename = paste0("ex_arbre_",k,".pdf"),
#        plot = gg,
#        device = "pdf",
#        width = 5,
#        height = 5)
# Thk <- struc$Tau.list[[k]]
# diag(Thk) <- 2
# pdf(file = paste0("ex_Th_",k,".pdf"),5,5)
# par(mar=c(0,0,0,0))
# image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
# dev.off()


attr(dend[[v[-1]]], "delta") <- 1
attr(dend[[c(v[-1],2)]], "delta") <- -4
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
ggsave(filename = paste0("ex_arbre_",k,"-2.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
Thk <- struc$Tau.list[[k]]
diag(Thk) <- 2
pdf(file = paste0("ex_Th_",k,".pdf"),5,5)
par(mar=c(0,0,0,0))
image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
dev.off()


k <- 33
dend <- struc$dend.begin.list[[k]]
dend <- validify(dend)
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
ggsave(filename = paste0("ex_arbre_",k-1,"-star0.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
attr(dend[[v[-1]]],"delta") <- 1
Thk <- constructTauTilde(dend,Tau.hat)
diag(Thk) <- 2

d <- 18
eps <- 1/(d-1)
ss <- seq(-eps/2,1+eps/2,length.out=d+1)
diag(Tau) <- 2

pdf(file = paste0("ex_Th_",k-1,"-star0.pdf"),5,5)
        par(mar=c(0,0,0,0))
        image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
        rect(ss[16],ss[1],ss[19],ss[4],lty=1,lwd=2)
dev.off()



attr(dend[[5]], "delta") <- 0
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
ggsave(filename = paste0("ex_arbre_",k-1,"-star1.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
attr(dend[[v[-1]]],"delta") <- 1
Thk <- constructTauTilde(dend,Tau.hat)
diag(Thk) <- 2

pdf(file = paste0("ex_Th_",k-1,"-star1.pdf"),5,5)
        par(mar=c(0,0,0,0))
        image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
        rect(ss[16],ss[1],ss[19],ss[4],lty=1,lwd=2)
dev.off()



k <- 33
dend <- struc$dend.begin.list[[k]]
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
ggsave(filename = paste0("ex_arbre_",k,".pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
Thk <- struc$Tau.list[[k]]
diag(Thk) <- 2
pdf(file = paste0("ex_Th_",k,".pdf"),5,5)
par(mar=c(0,0,0,0))
image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
dev.off()



source("thesis-application-3/functions/testInner.R")
colnames(X) <- as.character(1:18)
Tau.hat <- cor.fk(X)
testInner(node = struc$dend.begin.list[[32]][[v[-1]]], k=2, Tau.hat = Tau.hat, Tau.hajek = constructTauHajek(X))
