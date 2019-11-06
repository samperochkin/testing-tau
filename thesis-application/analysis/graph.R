library(ggraph)
library(igraph)

library(tidyverse)
library(RColorBrewer) 
# create a data frame giving the hierarchical structure of your individuals
vec.address <- getAddresses(dend)

to <- sapply(vec.address, paste0, collapse = "")
from <- sapply(vec.address, function(v) paste0(v[-length(v)], collapse = ""))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address, function(v){
  length(unlist(getSubDend(dend,v)))
})
tt <- constructTauTilde(dend, return.single.values = T)

vertices <- data.table(name = to,
                       group = from,
                       tt,
                       size)

maxlen <- max(sapply(vec.address,length))
aa <- sapply(vec.address, function(v){
  a <- as.integer(paste0(v,collapse = ""))
  a*10^(maxlen-length(v))
})
vertices <- vertices[order(aa, decreasing = T),]

vertices$id=NA
myleaves=which(is.na( match(vertices$name, vertices$group) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = 1:nleaves
vertices$angle = 360 - 360 * (2+vertices$id) / nleaves


vertices$hjust <- as.integer(vertices$angle %between% c(90,270))
vertices$angle <- ifelse(vertices$angle %between% c(90,270), vertices$angle+180, vertices$angle)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  # ggraph(mygraph, layout = 'partition', circular = TRUE) + 
  geom_edge_diagonal(colour="darkgrey") +
  # scale_edge_colour_distiller(palette = "RdPu") +
  # geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2.7, alpha=1) +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, angle=angle, label=id, hjust=hjust), alpha=1) +
  geom_node_point(aes(x = x*1.07, y=y*1.07, colour=tt, size=size), alpha = 1) +
  scale_colour_gradient(low = "lightblue", high = "darkred") +
  scale_size_continuous( range = c(1,10) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

# ggraph(mygraph, layout = 'circlepack') + 
#   geom_node_circle() +
#   theme_void()
