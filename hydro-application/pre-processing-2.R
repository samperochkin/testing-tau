library(dplyr)
library(tidyr)
library(readr)


stns <- read_csv("data/stns_pp1.csv")
data2 <- read_csv("data/data_pp1.csv")


ids <- as.numeric(names(data2)[-1])
ll <- stns %>% subset(ID %in% ids) %>% select(ID, Country, Lon., Lat.)

names(ll) <- c("ID","COUN","LON","LAT")

g1 <- ll %>% subset(LON == min(LON)) %>% select(ID) %>% pull
g2 <- ll %>% subset(between(LON,min(LON)+1,-102) & LAT < 50) %>% pull(ID)
g3 <- ll %>% subset(between(LON,min(LON)+1,-102) & LAT > 50) %>% pull(ID)
g4 <- ll %>% subset(COUN == "PAN") %>% pull(ID)
g5 <- ll %>% subset(between(LON,-102,0) & COUN == "USA") %>% pull(ID)
g6 <- ll %>% subset(between(LON,-102,0) & COUN == "CAN") %>% pull(ID)

gs <- list(g1,g2,g3,g4,g5,g6)

is <- lapply(gs, function(g){
  ii <- which(ll$ID %in% g)
  ii[order(ll$LAT[ii])]
})


#### map figure

ll2 <- ll[order(ll$LON),]
library(mapdata)
par(mfrow=c(1,1), mar=rep(0,4))
map("world", fill=TRUE,col="lightgray",xlim = range(ll$LON) + c(-3,15), ylim=range(ll$LAT) + c(-4,4))
points(ll2[,c("LON","LAT")], col = alpha(2,.5), pch = 19, cex = 2)
points(ll2[,c("LON","LAT")], col = 1, pch = 19, cex = .75)
text(ll2[,c("LON","LAT")], labels = 1:nrow(ll), pos = 2, cex = 1.2)



pdf("figures/map-stations.pdf", width = 7, height = 3.5)
par(mfrow=c(1,1), mar=rep(0,4))
map("world", fill=TRUE,col="lightgray",xlim = range(ll$LON) + c(-3,15), ylim=range(ll$LAT) + c(-4,4))

lab0 <- cumsum(c(0,sapply(is,length)))
poos <- c(3,
          2,2,2,4,3,
          2,3,
          3,
          2,4,3,4,2,4,3,1,
          3)
library(scales)
sapply(1:length(is), function(k){
  ii <- is[[k]]
  points(ll[ii,c("LON","LAT")], col = alpha(k+1,.5), pch = 19, cex = 2)
})
sapply(1:length(is), function(k){
  ii <- is[[k]]
  points(ll[ii,c("LON","LAT")], col = "black", pch = 19, cex = .75)
  text(ll[ii,c("LON","LAT")], labels = lab0[k] + 1:length(ii),
       col = 1,
       pos = poos[lab0[k] + 1:length(ii)],
       cex = .9)
})
dev.off()


# time series data
X <- as.matrix(data2[,as.character(ll[unlist(is),]$ID)])

par(mfrow = c(3,3), mar = c(2,2,1,1))
apply(X,2, function(x){
  plot(data2$YEAR, x)
  lines(data2$YEAR, x)
})

# Fit linear regressions
Y <- apply(X, 2, function(y){
  x <- data2$YEAR - min(data2$YEAR)
  summary(lm(y~x))$res
})  
rownames(Y) <- data2$YEAR
nrow(Y) - apply(Y,2, function(y) length(unique(y))) # no ties

# figures sea-levels
pdf("figures/sea-levels.pdf", width = 8, height = 1.6)
par(mfrow = c(1,4), mar=c(2,2,2,1))
sapply(c(1,8,10,18), function(i){
  id <- colnames(X)[i]
  nam <- stns %>% subset(ID == as.integer(id)) %>% pull(`Station Name`)
  cou <- stns %>% subset(ID == as.integer(id)) %>% pull(Country)
  y <- X[,i]
  x <- data2$YEAR - min(data2$YEAR)
  su <- summary(lm(y~x))
  plot(data2$YEAR,y,main=paste0(nam, ", ", cou, " (",i,")"))
  lines(data2$YEAR,y)
  lines(data2$YEAR,su$coefficients[1] + x*su$coefficients[2], col="blue")
})
dev.off()


K <- length(is)
ds <- sapply(is,length)
d <- sum(ds)
clus <- rep(0,d)
for(k in 1:K){
  clus[sum(c(0,ds)[1:k]) + 1:ds[k]] <- k
}

saveRDS(Y,"data/Y.rds")
saveRDS(clus,"data/clus.rds")


pal <- colorRampPalette(c("green3","lightgray","indianred4"))
D <- as.matrix(dist(ll[unlist(is),c("LON","LAT")]))
image(t(-D[d:1,]), col = c(pal(1000),"black"), xaxt="n", yaxt="n")

