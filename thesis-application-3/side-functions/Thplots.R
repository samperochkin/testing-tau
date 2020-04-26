pal <- wes_palette("Zissou1", 112, type = "continuous")
cols <- c(rep("white",88),pal,rep("black",100))


par(mfrow = c(1,3), mar=c(1,1,1,1))
sapply(1:3, function(k){
  Th <- struc$Taus[[k]]
  diag(Th) <- 2
  Th <- Th[1:5,1:18]
  
  image(t(Th[5:1,1:18]), col = cols, zlim = c(-1,2), axes = F)
  abline(v=4.75/18, lty = 2)
  abline(v=1.025, lty = 2)
  segments(x0=4.75/18,y0=1.125,x1=1.025,y1=1.125,lty=2)
  segments(x0=4.75/18,y0=-.12,x1=1.025,y1=-.12,lty=2)
})



cols <- c("Navy",rep("lightgray",87),pal,rep("black",100))
par(mfrow = c(1,4), mar=c(1,1,1,1))
sapply(1, function(k){
  Th <- struc$Taus[[k]]
  diag(Th) <- 2
  image(t(Th[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
  abline(v=4.75/18, lty = 2)
  abline(v=1.025, lty = 2)
  abline(h=1-4.75/18, lty = 2)
  abline(h=1.03, lty = 2)
})
sapply(1:3, function(k){
  Th <- struc$Taus[[k]]
  Th[6:18,1:18] <- -.5
  Th[1:2,3:5] <- -.5
  Th[3:5,1:2] <- -.5
  Th[1:2,1:2] <- -1
  Th[3:5,3:5] <- -1
  Th[6:7,6:7] <- -1
  Th[8:10,8:10] <- -1
  Th[11:15,11:15] <- -1
  diag(Th) <- 2
  
  image(t(Th[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
})


gg <- weirdPlot(dend,non.leaf,F)
ggsave(filename = paste0("arbre13-noleg.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)


dend2 <- dend

dend2 <- unbranch(dend2,2)
dend2 <- unbranch(dend2,1)
dend2 <- unbranch(dend2,1)


vec.address2 <- getAddresses(dend2)

to <- sapply(vec.address2, function(v) paste0("(",paste0(v, collapse = ","),")"))
from <- sapply(vec.address2, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address2, function(v){
  length(unlist(getSubDend(dend2,v)))
})
type <- sapply(vec.address2, function(v){
  node <- getSubDend(dend2,v)
  if(is.leaf(node)) return("feuille")
  if(identical(attr(node, "valid"),FALSE)) return("indéfini")
  if(identical(attr(node, "valid"),TRUE) & identical(attr(node, "type"),0)) return("hétérogène")
  if(identical(attr(node, "valid"),TRUE) | identical(attr(node, "type"),1)) return("homogène")
  # attr(node, "type")
  return("hétérogène")
})


type <- factor(type, levels = c("feuille","homogène","hétérogène","indéfini"))


tt <- constructTauTilde(dend2, Tau.hat, return.single.values = T) 

id1 <- sapply(vec.address2, function(v){
  node <- getSubDend(dend2,v)
  if(is.leaf(node)) return(attr(node,"label")) 
  else(return(NA))
})

id2 <- rep(NA,length(id1))
id2[is.na(id1)] <- to[is.na(id1)]


vertices <- data.table(name = to,
                       group = from,
                       id1,
                       id2,
                       tt,
                       size,
                       type=type)

maxlen <- max(sapply(vec.address2,length))
aa <- sapply(vec.address2, function(v){
  a <- as.integer(paste0(v,collapse = ""))
  a*10^(maxlen-length(v))
})
vertices <- vertices[order(aa),]
type.unique <- unique(vertices$type)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.75) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y, colour=type, filter=leaf), size = 2, alpha = 1) +
  geom_node_point(aes(x = x, y=y, colour=type, filter = !leaf), size = 5, alpha = 1) +
  theme_void() + 
  scale_color_manual(values = c("feuille" = pal2[1], "homogène" = pal2[2],
                                "hétérogène" = pal2[3], "indéfini" = pal2[4]),
                     limits = c("feuille","homogène","hétérogène","indéfini"))

  gg <- gg + theme(legend.position = "none") 

gg
ggsave(filename = paste0("arbre-W.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)


Tau.tilde <- constructTauTilde(dend2,Tau.hat)
diag(Tau.tilde) <- 2

oo[6:10] <- c(8,6,7,9,10)

pdf(width = 5, height = 5, file = "Tau-tilde-W.pdf")

par(mar=c(0,0,0,0))
image(t(Tau.tilde[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)

segments(y0=1-4.75/18,y1=1-18.5/18,x0=1-13.25/18,x1=1-13.25/18,lty=2)
segments(y0=1-4.75/18,y1=1-18.5/18,x0=1-18.5/18,x1=1-18.5/18,lty=2)
segments(x0=1-13.25/18,x1=1-18.5/18,y0=1-4.75/18,y1=1-4.75/18,lty=2)
segments(x0=1-13.25/18,x1=1-18.5/18,y0=1-18.5/18,y1=1-18.5/18,lty=2)

dev.off()
     

pdf(width = (2/18)*5, height = 5, file = "Tau-tilde2-W.pdf")
cols[1] <- "white" 
par(mar=c(0,0,0,0))
Tt <- Tau.tilde[oo[c(6,13,18)],oo[c(1,3)]]
Tt2 <- matrix(-1,18,2)
Tt2[8:10,] <- Tt
Tt2 <- Tt2[18:1,]
image(t(Tt2), col = cols, zlim = c(-1,2), axes = F)
dev.off()




dend2 <- dend
dend2 <- unbranch(dend2,1)


vec.address2 <- getAddresses(dend2)

to <- sapply(vec.address2, function(v) paste0("(",paste0(v, collapse = ","),")"))
from <- sapply(vec.address2, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address2, function(v){
  length(unlist(getSubDend(dend2,v)))
})
type <- sapply(vec.address2, function(v){
  node <- getSubDend(dend2,v)
  if(is.leaf(node)) return("feuille")
  if(identical(attr(node, "valid"),FALSE)) return("indéfini")
  if(identical(attr(node, "valid"),TRUE) & identical(attr(node, "type"),0)) return("hétérogène")
  if(identical(attr(node, "valid"),TRUE) | identical(attr(node, "type"),1)) return("homogène")
  # attr(node, "type")
  return("indéfini")
})


type <- factor(type, levels = c("feuille","homogène","hétérogène","indéfini"))


tt <- constructTauTilde(dend2, Tau.hat, return.single.values = T) 

id1 <- sapply(vec.address2, function(v){
  node <- getSubDend(dend2,v)
  if(is.leaf(node)) return(attr(node,"label")) 
  else(return(NA))
})

id2 <- rep(NA,length(id1))
id2[is.na(id1)] <- to[is.na(id1)]


vertices <- data.table(name = to,
                       group = from,
                       id1,
                       id2,
                       tt,
                       size,
                       type=type)

maxlen <- max(sapply(vec.address2,length))
aa <- sapply(vec.address2, function(v){
  a <- as.integer(paste0(v,collapse = ""))
  a*10^(maxlen-length(v))
})
vertices <- vertices[order(aa),]
type.unique <- unique(vertices$type)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.75) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y, colour=type, filter=leaf), size = 2, alpha = 1) +
  geom_node_point(aes(x = x, y=y, colour=type, filter = !leaf), size = 5, alpha = 1) +
  theme_void() + 
  scale_color_manual(values = c("feuille" = pal2[1], "homogène" = pal2[2],
                                "hétérogène" = pal2[3], "indéfini" = pal2[4]),
                     limits = c("feuille","homogène","hétérogène","indéfini"))

gg <- gg + theme(legend.position = "none") 

gg
ggsave(filename = paste0("arbre14-noleg.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)

