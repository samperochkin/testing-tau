library(maps)
library(dplyr)

ll <- stns %>% subset(`Station Name` %in% colnames(Y)) %>% select(ID, Country, Lon., Lat., `Station Name`)
names(ll) <- c("ID","COUN","LON","LAT","NAME")
ll <- ll %>% slice(match(colnames(Y),ll$NAME))
poos <- c(3,
          2,2,2,4,3,
          2,3,
          3,
          2,4,3,4,2,4,3,1,
          3)


clus <- clus.mat[,2]
K <- length(unique(clus))
pal <- rainbow(K)
pie(rep(1,K),col=pal)


pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/map-stations-nes1.pdf", width = 7, height = 3.5)
par(mfrow=c(1,1))
map("world",
    mar=rep(0,4),
    fill=TRUE,col="lightgray",xlim = range(ll$LON) + c(-3,15), ylim=range(ll$LAT) + c(-4,4))

sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = pal[k], pch = 19, cex = 2)
})
sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = "black", pch = 19, cex = .75)
  # text(ll[ii,c("LON","LAT")], labels = ii,
  #      col = 1,
  #      pos = poos[ii],
  #      cex = .9)
})
dev.off()



clus <- clus.mat[,4]
clus[clus.mat[,2] == 2] <- match(clus[clus.mat[,2] == 2],unique(clus[clus.mat[,2] == 2]))
clus[clus.mat[,2] != 2] <- max(clus[clus.mat[,2] == 2])+1

K <- length(unique(clus))
pie(rep(1,K-1),col=rainbow(K-1))
pal <- c(rainbow(K-1),"black")


pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/map-stations-nes2.pdf", width = 7, height = 3.5)
par(mfrow=c(1,1))
map("world",
    mar=rep(0,4),
    fill=TRUE,col="lightgray",xlim = range(ll$LON) + c(-3,15), ylim=range(ll$LAT) + c(-4,4))

sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = pal[k], pch = 19, cex = 2)
})
sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = "black", pch = 19, cex = .75)
  # text(ll[ii,c("LON","LAT")], labels = ii,
  #      col = 1,
  #      pos = poos[ii],
  #      cex = .9)
})
dev.off()




clus <- clus.mat[,6]
clus[clus.mat[,4] == 2] <- match(clus[clus.mat[,4] == 2],unique(clus[clus.mat[,4] == 2]))
clus[clus.mat[,4] != 2] <- max(clus[clus.mat[,4] == 2])+1

K <- length(unique(clus))
pie(rep(1,K-1),col=rainbow(K-1))
pal <- c(rainbow(K-1),"black")


pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/map-stations-nes3.pdf", width = 7, height = 3.5)
par(mfrow=c(1,1))
map("world",
    mar=rep(0,4),
    fill=TRUE,col="lightgray",xlim = range(ll$LON) + c(-3,15), ylim=range(ll$LAT) + c(-4,4))

sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = pal[k], pch = 19, cex = 2)
})
sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = "black", pch = 19, cex = .75)
  # text(ll[ii,c("LON","LAT")], labels = ii,
  #      col = 1,
  #      pos = poos[ii],
  #      cex = .9)
})
dev.off()








clus <- clus.mat[,3]
clus[clus.mat[,2] == 4] <- match(clus[clus.mat[,2] == 4],unique(clus[clus.mat[,2] == 4]))
clus[clus.mat[,2] != 4] <- max(clus[clus.mat[,2] == 4])+1

K <- length(unique(clus))
pie(rep(1,K-1),col=rainbow(K-1))
pal <- c(rainbow(K-1),"black")

pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/map-stations-nes4.pdf", width = 7, height = 3.5)
par(mfrow=c(1,1))
map("world",
    mar=rep(0,4),
    fill=TRUE,col="lightgray",xlim = range(ll$LON) + c(-3,15), ylim=range(ll$LAT) + c(-4,4))

sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = pal[k], pch = 19, cex = 2)
})
sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = "black", pch = 19, cex = .75)
  # text(ll[ii,c("LON","LAT")], labels = ii,
  #      col = 1,
  #      pos = poos[ii],
  #      cex = .9)
})
dev.off()





clus <- clus.mat[,5]
clus[clus.mat[,3] == 4] <- match(clus[clus.mat[,3] == 4],unique(clus[clus.mat[,3] == 4]))
clus[clus.mat[,3] != 4] <- max(clus[clus.mat[,3] == 4])+1

K <- length(unique(clus))
pie(rep(1,K-1),col=rainbow(K-1))
pal <- c(rainbow(K-1),"black")

pdf("C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/map-stations-nes5.pdf", width = 7, height = 3.5)
par(mfrow=c(1,1))
map("world",
    mar=rep(0,4),
    fill=TRUE,col="lightgray",xlim = range(ll$LON) + c(-3,15), ylim=range(ll$LAT) + c(-4,4))

sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = pal[k], pch = 19, cex = 2)
})
sapply(unique(clus), function(k){
  ii <- which(clus == k)
  points(ll[ii,c("LON","LAT")], col = "black", pch = 19, cex = .75)
  # text(ll[ii,c("LON","LAT")], labels = ii,
  #      col = 1,
  #      pos = poos[ii],
  #      cex = .9)
})
dev.off()



##### STRUCT ####

library(ggraph)
library(igraph)
invisible(sapply(list.files("thesis-application-3/double/functions/", full.names = T), source, local = environment()))

dend <- struc$dend
plot(dend)
dend <- rotate(dend,order(order.dendrogram(dend)))
plot(dend)

true.labels <- labels(dend)
vec.address <- getAddresses(dend)

to <- sapply(vec.address, function(v) paste0("(",paste0(v, collapse = ","),")"))
from <- sapply(vec.address, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address, function(v){
  length(unlist(getSubDend(dend,v)))
})
delta <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
  return(attr(node, "delta"))
  # if(is.leaf(node)) return("feuille") 
  # if(attr(node, "delta")==1) return("homogène")
  # if(attr(node, "delta")==0) return("hétérogène")
})
delta[delta==-2] <- 0
delta <- factor(delta, levels = c(-1,0,1), labels = c("feuille","hétérogène","homogène"))
tt <- constructTauTilde(dend, cor.fk(Y), return.single.values = T)
id1 <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
  if(is.leaf(node)) return(attr(node,"label")) 
  else(return(NA))
})

id2 <- rep(NA,length(id1))
id2[is.na(id1)] <- to[is.na(id1)]

# tick <- colnames(X)[as.numeric(id1)]
# tick <- colnames(X)[as.numeric(id1)]
tick <- id1


vertices <- data.table(name = to,
                       group = from,
                       id1,
                       id2,
                       tick,
                       tt,
                       size,
                       delta=delta)

maxlen <- max(sapply(vec.address,length))
aa <- sapply(vec.address, function(v){
  a <- as.integer(paste0(v,collapse = ""))
  print(a)
  a*10^(maxlen-length(v))
})
vertices <- vertices[order(aa),]
delta.unique <- unique(vertices$delta)

print(delta)


# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )
pal <- c("#000000", "#009E73", "#D55E00")

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F)

gg <- gg + 
  geom_edge_diagonal(colour="black",strength = .8) +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=tick),
                 angle = -90, hjust = 0, alpha=1,
                 nudge_x = .1, nudge_y = -.2, size = 2.5) +
  # geom_node_text(aes(x = x, y=y, filter = !leaf, label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y-.045, color=delta, filter=!leaf), size = 3, alpha = 1) +
  geom_node_point(aes(x = x, y=y, color=delta, filter=leaf), size = 1.5, alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = c(.85,.8)) +
  # guides(color = guide_legend(override.aes = list(size=3)), size = F) +
  scale_color_manual(values = pal[c(1,3,2)],
                     guide = guide_legend(title.position = "right",
                                          title.theme = element_text(angle=-90),
                                          label.theme = element_text(angle=-90),
                                          label.position = "bottom",
                                          direction = "horizontal", reverse = T,label.hjust = 0)) +
  xlim(0,d-1+.1) + ylim(-5,max(sapply(vec.address,length))-1)


gg


gg <- ggraph(mygraph, layout = 'dendrogram', circular = F)
gg <- gg + 
  geom_edge_diagonal(colour="black",strength = .8) +
  geom_node_point(aes(x = x, y=y-.045, color=delta, filter=!leaf), size = 4, alpha = 1) +
  geom_node_point(aes(x = x, y=y, color=delta, filter=leaf), size = 2, alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.position="none") +
  scale_color_manual(values = pal[c(1,3,2)]) +
  xlim(0,d-1+.1)

gg

ggsave(filename = "C:/Users/Samuel/OneDrive - Université Laval/Samuel-Perreault-111038169/soutenance/Figures-new/dend-nes.pdf",
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
