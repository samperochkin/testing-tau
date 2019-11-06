library(dendextend)
library(data.table)
library(pcaPP)
library(Matrix)
library(mvtnorm)

#### Data ####

X <- readRDS("application/returns_mat.rds")
#X <- X[nrow(X) + (-51:0),]
meta <- fread("application/NASDAQ100_meta.csv")

table(meta[,Sector])



#### Setup ####

sapply(list.files("thesis-application/functions/", full.names = T), source)
sapply(list.files("thesis-application/vizu/", full.names = T), source)
source("thesis-application/setup/setup.R")

M <- 2000
alpha <- .05


dend <- initialObjects(Tau.hat,method = "average")$dend

dend <- loopHetero(dend)
plot(dend)

dend <- loopHomo(dend)
plot(dend)


dend <- tauCalculator(dend, Tau.hat, oorder = T)
Tau.tilde <- constructTauTilde(dend)

oo <- unlist(dend)
pal <- wes_palette("Zissou1", 100, type = "continuous")
par(mfrow = c(1,2), mar = c(1,1,1,1))
image(t(Tau.hat[rev(oo),oo]), col=pal)
image(t(Tau.tilde[rev(oo),oo]), col=pal)


saveRDS(dend,"thesis-application/real/final-dend.rds")

