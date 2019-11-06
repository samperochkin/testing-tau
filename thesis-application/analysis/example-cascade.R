library(data.table)
library(ggraph)
library(igraph)

library(tidyverse)
library(RColorBrewer) 
library(dendextend) 

source("thesis-application/setup/constructFun.R")
sapply(list.files("thesis-application/functions",full.names = T), source)


set.seed(seed)
X <- rmvnorm(n=85, sigma = sin(pi*Tau/2))
Tau.hat <- cor.fk(X)

#### fake one

# Tau.hat <- Tau
d <- ncol(Tau)
set.seed(666)
# hc <- hclust(as.dist(1-Tau+matrix(runif(d^2,-.05,.05),d,d)),"single")
hc <- hclust(as.dist(1-Tau.hat),"single")
plot(hc)

dend <- as.dendrogram(hc)
dend <- dend[[c(1,1)]]



vec.address <- getAddresses(dend)
for(v in rev(vec.address)){
  node <- getSubDend(dend,v)
  node[1:length(node)] <- node[order(sapply(node,function(nn) min(unlist(nn))))]
  dend <- assignSubDend(node,dend,v)
}
vec.address <- getAddresses(dend)

plot(dend)


to <- sapply(vec.address, function(v) paste0("(",paste0(v, collapse = ","),")"))
from <- sapply(vec.address, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address, function(v){
  length(unlist(getSubDend(dend,v)))
})
type <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
  if(is.leaf(node)) return("feuille") 
  if(length(node) == 2) return("homogène")
  attr(node, "type")
})
type <- factor(type, levels = c("feuille","homogène","hétérogène"))
tt <- constructTauTilde(dend, return.single.values = T)
id1 <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
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

maxlen <- max(sapply(vec.address,length))
aa <- sapply(vec.address, function(v){
  a <- as.integer(paste0(v,collapse = ""))
  a*10^(maxlen-length(v))
})
vertices <- vertices[order(aa),]
type.unique <- unique(vertices$type)


# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

pal <- c("#000000", "#009E73", "#D55E00")
# c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.5, vjust = 1, size = 6) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1, nudge_x = -.25, angle = -30) +
  geom_node_point(aes(x = x, y=y, color=type), size = 5) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.position = "none") +
  # theme(legend.text=element_text(size=17),
  #       legend.title=element_text(size=17)) +
  # theme(legend.position = c(.23,.84)) +
  # guides(color = guide_legend(override.aes = list(size=8)), alpha = F) +
  scale_colour_manual(values = pal,
                      labels = levels(type),
                      limits = levels(type),
                      drop = FALSE)

gg

pdf(file = "cascade3.pdf", width = 5, height = 5)
gg
dev.off()




#### TRUE


Tau.hat <- Tau
d <- ncol(Tau)
hc <- hclust(as.dist(1-Tau),"single")
plot(hc)

dend <- as.dendrogram(hc)
dend <- dend[[c(1,1)]]



vec.address <- getAddresses(dend)
for(v in rev(vec.address)){
  node <- getSubDend(dend,v)
  node[1:length(node)] <- node[order(sapply(node,function(nn) min(unlist(nn))))]
  dend <- assignSubDend(node,dend,v)
}
vec.address <- getAddresses(dend)

plot(dend)


to <- sapply(vec.address, function(v) paste0("(",paste0(v, collapse = ","),")"))
from <- sapply(vec.address, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address, function(v){
  length(unlist(getSubDend(dend,v)))
})
type <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
  if(is.leaf(node)) return("feuille") 
  if(length(node) == 2) return("homogène")
  attr(node, "type")
})
type <- factor(type, levels = c("feuille","homogène","hétérogène"))
tt <- constructTauTilde(dend, return.single.values = T)
id1 <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
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

maxlen <- max(sapply(vec.address,length))
aa <- sapply(vec.address, function(v){
  a <- as.integer(paste0(v,collapse = ""))
  a*10^(maxlen-length(v))
})
vertices <- vertices[order(aa),]
type.unique <- unique(vertices$type)


# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

pal <- c("#000000", "#009E73", "#D55E00")
# c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.5, vjust = 1, size = 6) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1, nudge_x = -.25, angle = -30) +
  geom_node_point(aes(x = x, y=y, color=type), size = 5) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  # theme(legend.position = "none") +
  theme(legend.text=element_text(size=17),
        legend.title=element_text(size=17)) +
  theme(legend.position = c(.23,.84)) +
  guides(color = guide_legend(override.aes = list(size=8)), alpha = F) +
  scale_colour_manual(values = pal,
                      labels = levels(type),
                      limits = levels(type),
                      drop = FALSE)

gg

pdf(file = "cascade2.pdf", width = 5, height = 5)
gg
dev.off()



#######################



dend <- unbranch(dend,1)
dend <- unbranch(dend,1)
dend <- unbranch(dend,1)
attr(dend, "type") <- "homogène"



vec.address <- getAddresses(dend)

to <- sapply(vec.address, function(v) paste0("(",paste0(v, collapse = ","),")"))
from <- sapply(vec.address, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address, function(v){
  length(unlist(getSubDend(dend,v)))
})
type <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
  if(is.leaf(node)) return("feuille") 
  if(length(node) == 2) return("homogène")
  attr(node, "type")
})
type <- factor(type, levels = c("feuille","homogène","hétérogène"))
tt <- constructTauTilde(dend, return.single.values = T)
id1 <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
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

maxlen <- max(sapply(vec.address,length))
aa <- sapply(vec.address, function(v){
  a <- as.integer(paste0(v,collapse = ""))
  a*10^(maxlen-length(v))
})
vertices <- vertices[order(aa),]
type.unique <- unique(vertices$type)


# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.5, vjust = 1, size = 6) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1, nudge_x = -.25, angle = -30) +
  geom_node_point(aes(x = x, y=y, color=type), size = 5) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.position = "none") +
  scale_colour_manual(values = pal,
                      labels = levels(type),
                      limits = levels(type),
                      drop = FALSE) #+


gg

pdf(file = "cascade1.pdf", width = 5, height = 5)
gg
dev.off()
