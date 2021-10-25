# library(dendextend)
library(data.table)
# library(pcaPP)
# library(Matrix)
# library(mvtnorm)
library(quantmod)

#### Data ####


load("application/quotes.Rdata")
range(index(NASDAQ100_quotes$AAL))

X <- readRDS("application/returns_mat.rds")
range(rownames(X))

index(NASDAQ100_quotes$AAL)[which(index(NASDAQ100_quotes$AAL) %in% rownames(X))]
rm(NASDAQ100_quotes)

#X <- X[nrow(X) + (-51:0),]
meta <- fread("application/NASDAQ100_meta.csv")

table(meta[,Sector])
table(meta[,industry])




# Run Algorithm -----------------------------------------------------------
Tau.hat <- pcaPP::cor.fk(X)

# source("thesis-application-3/structureBuilder.R")
source("thesis-application-3/double/structureBuilder2.R")
struc <- structureBuilder(X=X,
                         hclust_method = "mcquitty",
                         alpha0 = .05,
                         alpha1 = .05,
                         M = 2000)

dend <- struc$dend
Tau.tilde <- struc$Tau.tilde

plot(dend)
saveRDS(dend,"thesis-application-3/real/dend-final.rds")

# dend <- tauCalculator(dend, Tau.hat, oorder = T)
# Tau.tilde <- constructTauTilde(dend)
rownames(Tau.tilde) <- colnames(Tau.tilde) <- rownames(Tau.hat)

oo <- unlist(dend)
library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")
par(mfrow = c(1,2), mar = c(1,1,1,1))
ran <- range(c(Tau.hat[t(combn(nrow(Tau.hat),2))]))
image(t(Tau.hat[rev(oo),oo]), col=pal, zlim = ran)
image(t(Tau.tilde[rev(oo),oo]), col=pal, zlim = ran)


par(mfrow = c(2,1), mar = c(.5,1,.5,1))
image(t(Tau.hat[rev(oo),oo]), col = pal, zlim = c(-.2,1), axes = F)
image(t(Tau.tilde[rev(oo),oo]), col = pal, zlim = c(-.2,1), axes = F)

# saveRDS(dend,"thesis-application/real/final-dend.rds")

