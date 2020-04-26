source("thesis-application-3/functions/validify.R")

k <- 31
dend <- struc$dend.begin.list[[k]]
v <- struc$v.list[[k]]
attr(dend[[v[-1]]], "delta") <- -3
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


dend <- validify(dend)
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
ggsave(filename = paste0("ex_arbre_",k,"-star0.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
attr(dend[[v[-1]]],"delta") <- 1
Thk <- constructTauTilde(dend,Tau.hat)
diag(Thk) <- 2
pdf(file = paste0("ex_Th_",k,"-star0.pdf"),5,5)
par(mar=c(0,0,0,0))
image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

attr(dend[[v[length(v)]]],"delta") <- -2
dend <- validify(dend)
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
ggsave(filename = paste0("ex_arbre_",k,"-star1.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
Thk <- constructTauTilde(dend,Tau.hat)
diag(Thk) <- 2
pdf(file = paste0("ex_Th_",k,"-star1.pdf"),5,5)
par(mar=c(0,0,0,0))
image(t(Thk[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
dev.off()




k <- 32
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

