library(data.table)

Y <- readRDS("/home/samperochkin/Dropbox/Academia/Papers/Hierarchical archimax/clustering/secretData")
Y <- readRDS("C:/Users/Samuel/Gits/hydrometric-app/data/Y.rds")
stns <- fread("C:/Users/Samuel/Gits/hydrometric-app/data/rvested-stations.csv")

stns[ID %in% colnames(Y),]$ID

colnames(Y) <- stns[ID %in% colnames(Y),]$`Station Name`[match(colnames(Y),stns[ID %in% colnames(Y),]$ID)]

source("thesis-application-3/double/structureBuilder2.R")
struct <- structureBuilder(X=Y,
                           hclust_method = "mcquitty",
                           alpha0 = .05,
                           alpha1 = .05,
                           M = 2000)


par(mar=c(15,2,2,2))
plot(struct$dend)

id <- unlist(struct$dend)

oo <- sample(18)
image(t(cor.fk(Y)[rev(oo),oo]), zlim=c(-.1,1))

par(mfrow=c(1,2), mar=c(1,1,1,1))
image(t(cor.fk(Y)[rev(id),id]), zlim=c(-.1,1))
image(t(struct$Tau.tilde[rev(id),id]), zlim=c(-.1,1))


par(mfrow=c(1,2), mar=c(1,1,1,1))
image(t(cor.fk(Y)[18:1,]), zlim=c(-.1,1), axes=F)
image(t(struct$Tau.tilde[18:1,]), zlim=c(-.1,1), axes=F)

pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/Taus-nes.pdf", width = 8, height = 4)
par(mfrow=c(1,2), mar=c(1,1,1,1))
image(t(cor.fk(Y)[18:1,]), zlim=c(-.1,1), axes=F)
image(t(struct$Tau.tilde[18:1,]), zlim=c(-.1,1), axes=F)
dev.off()


ss <- seq(-1/(2*(d-1)),1+1/(2*(d-1)),length.out=d+1)
pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/Taus-nes2.pdf", width = 8, height = 4)
par(mfrow=c(1,2), mar=c(1,1,1,1))
image(t(cor.fk(Y)[18:1,]), zlim=c(-.1,1), axes=F)

# outer block
segments(x0=ss[2],y0=ss[19],x1=ss[19],y1=ss[19])
segments(x0=ss[2],y0=ss[18],x1=ss[9],y1=ss[18])
segments(x0=ss[9],y0=ss[11],x1=ss[10],y1=ss[11])
segments(x0=ss[10],y0=1-ss[10],x1=ss[18],y1=1-ss[10])
segments(x0=ss[18],y0=ss[2],x1=ss[19],y1=ss[2])

segments(x0=ss[2],y0=ss[19],x1=ss[2],y1=ss[18])
segments(x0=ss[9],y0=ss[18],x1=ss[9],y1=ss[11])
segments(x0=ss[10],y0=ss[11],x1=ss[10],y1=ss[10])
segments(x0=ss[18],y0=ss[10],x1=ss[18],y1=ss[2])
segments(x0=ss[19],y0=ss[19],x1=ss[19],y1=ss[2])


# other homogeneous
segments(x0=ss[15],y0=ss[5],x1=ss[16],y1=ss[5])
segments(x0=ss[16],y0=ss[4],x1=ss[17],y1=ss[4])

segments(x0=ss[15],y0=ss[6],x1=ss[15],y1=ss[5])
segments(x0=ss[16],y0=ss[5],x1=ss[16],y1=ss[4])

# rest
clus.list <- list(c(NA,1,2,3,4,4),
                  c(NA,1,1,1,1,1,2,2),
                  c(rep(NA,4),1,2),
                  c(rep(NA,6),1,2),
                  c(rep(NA,9),1,2,2,3),
                  c(rep(NA,9),1,1,1,1,2,2,2,3),
                  c(rep(NA,10),1,2)
)
sapply(clus.list, function(clus){
  K <- length(unique(clus))- any(is.na(clus))
  rs.mat <- cbind(unlist(sapply(1:K, function(i) seq(i)[-i])),
                  unlist(sapply(2:K, function(i) rep(i,i-1))))
  ss2 <- ss[c(sapply(1:K, function(k) which(clus==k)[1]),length(clus)+1)]
  sapply(1:nrow(rs.mat),function(k){
    r <- rs.mat[k,1]
    s <- rs.mat[k,2]
    
    rect(ss2[s],1-ss2[r],ss2[s+1],1-ss2[r+1])
  })
})



image(t(struct$Tau.tilde[18:1,]), zlim=c(-.1,1), axes=F)

# outer block
segments(x0=ss[2],y0=ss[19],x1=ss[19],y1=ss[19])
segments(x0=ss[2],y0=ss[18],x1=ss[9],y1=ss[18])
segments(x0=ss[9],y0=ss[11],x1=ss[10],y1=ss[11])
segments(x0=ss[10],y0=1-ss[10],x1=ss[18],y1=1-ss[10])
segments(x0=ss[18],y0=ss[2],x1=ss[19],y1=ss[2])

segments(x0=ss[2],y0=ss[19],x1=ss[2],y1=ss[18])
segments(x0=ss[9],y0=ss[18],x1=ss[9],y1=ss[11])
segments(x0=ss[10],y0=ss[11],x1=ss[10],y1=ss[10])
segments(x0=ss[18],y0=ss[10],x1=ss[18],y1=ss[2])
segments(x0=ss[19],y0=ss[19],x1=ss[19],y1=ss[2])


# other homogeneous
segments(x0=ss[15],y0=ss[5],x1=ss[16],y1=ss[5])
segments(x0=ss[16],y0=ss[4],x1=ss[17],y1=ss[4])

segments(x0=ss[15],y0=ss[6],x1=ss[15],y1=ss[5])
segments(x0=ss[16],y0=ss[5],x1=ss[16],y1=ss[4])

# rest
clus.list <- list(c(NA,1,2,3,4,4),
                  c(NA,1,1,1,1,1,2,2),
                  c(rep(NA,4),1,2),
                  c(rep(NA,6),1,2),
                  c(rep(NA,9),1,2,2,3),
                  c(rep(NA,9),1,1,1,1,2,2,2,3),
                  c(rep(NA,10),1,2)
)
sapply(clus.list, function(clus){
  K <- length(unique(clus))- any(is.na(clus))
  rs.mat <- cbind(unlist(sapply(1:K, function(i) seq(i)[-i])),
                  unlist(sapply(2:K, function(i) rep(i,i-1))))
  ss2 <- ss[c(sapply(1:K, function(k) which(clus==k)[1]),length(clus)+1)]
  sapply(1:nrow(rs.mat),function(k){
    r <- rs.mat[k,1]
    s <- rs.mat[k,2]
    
    rect(ss2[s],1-ss2[r],ss2[s+1],1-ss2[r+1])
  })
})


dev.off()



pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/Taus-nes3.pdf", width = 4, height = 4)
par(mfrow=c(1,1), mar=c(1,1,1,1))
image(t(struct$Tau.tilde[18:1,]), zlim=c(-.1,1), axes=F)

# outer block
segments(x0=ss[2],y0=ss[19],x1=ss[19],y1=ss[19])
segments(x0=ss[2],y0=ss[18],x1=ss[9],y1=ss[18])
segments(x0=ss[9],y0=ss[11],x1=ss[10],y1=ss[11])
segments(x0=ss[10],y0=1-ss[10],x1=ss[18],y1=1-ss[10])
segments(x0=ss[18],y0=ss[2],x1=ss[19],y1=ss[2])

segments(x0=ss[2],y0=ss[19],x1=ss[2],y1=ss[18])
segments(x0=ss[9],y0=ss[18],x1=ss[9],y1=ss[11])
segments(x0=ss[10],y0=ss[11],x1=ss[10],y1=ss[10])
segments(x0=ss[18],y0=ss[10],x1=ss[18],y1=ss[2])
segments(x0=ss[19],y0=ss[19],x1=ss[19],y1=ss[2])


# other homogeneous
segments(x0=ss[15],y0=ss[5],x1=ss[16],y1=ss[5])
segments(x0=ss[16],y0=ss[4],x1=ss[17],y1=ss[4])

segments(x0=ss[15],y0=ss[6],x1=ss[15],y1=ss[5])
segments(x0=ss[16],y0=ss[5],x1=ss[16],y1=ss[4])

# rest
clus.list <- list(c(NA,1,2,3,4,4),
                  c(NA,1,1,1,1,1,2,2),
                  c(rep(NA,4),1,2),
                  c(rep(NA,6),1,2),
                  c(rep(NA,9),1,2,2,3),
                  c(rep(NA,9),1,1,1,1,2,2,2,3),
                  c(rep(NA,10),1,2)
)
sapply(clus.list, function(clus){
  K <- length(unique(clus))- any(is.na(clus))
  rs.mat <- cbind(unlist(sapply(1:K, function(i) seq(i)[-i])),
                  unlist(sapply(2:K, function(i) rep(i,i-1))))
  ss2 <- ss[c(sapply(1:K, function(k) which(clus==k)[1]),length(clus)+1)]
  sapply(1:nrow(rs.mat),function(k){
    r <- rs.mat[k,1]
    s <- rs.mat[k,2]
    
    rect(ss2[s],1-ss2[r],ss2[s+1],1-ss2[r+1])
  })
})


dev.off()



dendextend:: struct$dend
clus.mat <- sapply(1:ncol(Y), function(k) cutree(struct$dend,k))
clus.mat <- clus.mat[,clus.mat[1,] != 0]
saveRDS(clus.mat, "thesis-application-3/clusmat.rds")

