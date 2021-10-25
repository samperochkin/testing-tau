library(data.table)
library(quantmod)

#### Data ####

load("application/quotes.Rdata")
range(index(NASDAQ100_quotes$AAL))

X <- readRDS("application/returns_mat.rds")
dend <- readRDS("thesis-application-3/real/final-dend.rds")

oo <- labels(dend)

Tau.hat <- pcaPP::cor.fk(X)

Tau.hat[oo,oo]

library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")
image(t(Tau.hat[rev(oo),oo]), col = pal, zlim = c(-.2,1), axes = F)
image(t(Tau.tilde[rev(oo),oo]), col = pal, zlim = c(-.2,1), axes = F)

source("thesis-application-3/double/functions/constructTauTilde.R")
Tau.tilde <- constructTauTilde(dend,Tau.hat)



par(mfrow = c(2,1), mar = c(.5,1,.5,1))
image(t(Tau.hat[rev(oo),oo]), col = pal, zlim = c(-.2,1), axes = F)
image(t(Tau.tilde[rev(oo),oo]), col = pal, zlim = c(-.2,1), axes = F)



