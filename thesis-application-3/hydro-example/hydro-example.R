library(data.table)

Y <- readRDS("C:/Users/Samuel/Dropbox/Git Projects/hydrometric-app/data/Y.rds")
stns <- fread("C:/Users/Samuel/Dropbox/Git Projects/hydrometric-app/data/rvested-stations.csv")

stns[ID %in% colnames(Y),]$ID

colnames(Y) <- stns[ID %in% colnames(Y),]$`Station Name`[match(colnames(Y),stns[ID %in% colnames(Y),]$ID)]

source("thesis-application-3/structureBuilder.R")
struct <- structureBuilder(X=Y,
                           hclust_method = "single",
                           alpha = .05,
                           M = 1000)


par(mar=c(15,2,2,2))
plot(struct$dend)

id <- unlist(struct$dend)

oo <- sample(18)
image(t(cor.fk(Y)[rev(oo),oo]), zlim=c(-.1,1))

par(mfrow=c(1,2), mar=c(1,1,1,1))
image(t(cor.fk(Y)[rev(id),id]), zlim=c(-.1,1))
image(t(struct$Tau.tilde[rev(id),id]), zlim=c(-.1,1))

image(t(cor.fk(Y)[18:1,]), zlim=c(-.1,1))
image(t(struct$Tau.tilde[18:1,]), zlim=c(-.1,1))


dendextend:: struct$dend
clus.mat <- sapply(1:ncol(Y), function(k) cutree(struct$dend,k))
clus.mat <- clus.mat[,clus.mat[1,] != 0]
saveRDS(clus.mat, "thesis-application-3/clusmat.rds")

