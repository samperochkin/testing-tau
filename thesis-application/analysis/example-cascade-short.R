library(data.table)
library(ggraph)
library(igraph)

library(tidyverse)
library(RColorBrewer) 
library(dendextend) 

pal <- c("#000000", "#009E73", "#D55E00")
# c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("thesis-application/setup/constructFun.R")
sapply(list.files("thesis-application/functions",full.names = T), source)

Tau.hat <- Tau
d <- ncol(Tau)


#######################

set.seed(777+2)
hc <- hclust(as.dist(1-Tau-runif(d^2,-.01,.01)),"single")
plot(hc)

dend <- as.dendrogram(hc)
dend <- dend[[c(1,1)]]

vec.address <- getAddresses(dend)
for(v in rev(vec.address)){
  node <- getSubDend(dend,v)
  node[1:length(node)] <- node[order(sapply(node,function(nn) min(unlist(nn))))]
  dend <- assignSubDend(node,dend,v)
}

plot(dend)

# Create a graph object
source("thesis-application/vizu/createGraph.R", encoding = "utf-8")
mygraph <- createGraph(dend)


gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.5, vjust = 1, size = 6) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1, nudge_x = -.25, angle = -30) +
  geom_node_point(aes(x = x, y=y, color=type), size = 5) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.position = "none") +
  scale_colour_manual(values = pal)

gg

pdf(file = "cascade3.pdf", width = 5, height = 5)
gg
dev.off()


####

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


mygraph <- createGraph(dend)

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.5, vjust = 1, size = 6) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1, nudge_x = -.25, angle = -30) +
  geom_node_point(aes(x = x, y=y, color=type), size = 5) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.position = "none") +
  scale_colour_manual(values = pal)
# ,
#                       labels = c("feuille","homogène","hétérogène"),
#                       limits = c("feuille","homogène","hétérogène"),
#                       drop = FALSE)

gg

pdf(file = "cascade2.pdf", width = 5, height = 5)
gg
dev.off()



#######################


dend <- unbranch(dend,1)
dend <- unbranch(dend,1)
dend <- unbranch(dend,1)
attr(dend, "type") <- "homogène"


# Create a graph object
mygraph <- createGraph(dend)


gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.5, vjust = 1, size = 6) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1, nudge_x = -.25, angle = -30) +
  geom_node_point(aes(x = x, y=y, color=type), size = 5) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=17),
        legend.title=element_text(size=17)) +
  theme(legend.position = c(.215,.87)) +
  guides(color = guide_legend(override.aes = list(size=8)), alpha = F) +
  scale_colour_manual(values = pal)
  #                     labels = c("feuille","homogène","hétérogène"),
  #                     limits = c("feuille","hétérogène","homogène"))
                      # drop = FALSE)


gg

pdf(file = "cascade1.pdf", width = 5, height = 5)
gg
dev.off()
