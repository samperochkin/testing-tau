dend <- struc$dends[[1]]

vec.address <- getAddresses(dend)
for(v in rev(vec.address)){
  node <- getSubDend(dend,v)
  node[1:length(node)] <- node[order(sapply(node,function(nn) min(unlist(nn))))]
  dend <- assignSubDend(node,dend,v)
}

dend <- dend[[c(1,2)]]

mygraph <- createGraph(dend, struc$Taus[[1]])

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.5, vjust = 1, size = 6) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1, nudge_x = -.25, angle = -30) +
  geom_node_point(aes(x = x, y=y, filter=leaf, color=type), size = 3) +
  geom_node_point(aes(x = x, y=y, filter=!leaf, color=type), size = 6) +
  # scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.position = "none") +
  scale_colour_manual(values = pal2)

gg

ggsave(filename = "cascade3.pdf",
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)


####

hc <- hclust(as.dist(1-Tau),"single")
dend <- as.dendrogram(hc)
vec.address <- getAddresses(dend)
for(v in rev(vec.address)){
  node <- getSubDend(dend,v)
  node[1:length(node)] <- node[order(sapply(node,function(nn) min(unlist(nn))))]
  dend <- assignSubDend(node,dend,v)
}
dend <- dend[[c(1,1,2)]]

mygraph <- createGraph(dend, skip = T)

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.5, vjust = 1, size = 6) +
  geom_node_point(aes(x = x, y=y, filter=leaf, color=type), size = 3) +
  geom_node_point(aes(x = x, y=y, filter=!leaf, color=type), size = 6) +
  # scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.position = "none") +
  scale_colour_manual(values = pal2)

gg

ggsave(filename = "cascade2.pdf",
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)



#######################


dend <- unbranch(dend,1)
dend <- unbranch(dend,1)
dend <- unbranch(dend,1)
attr(dend, "type") <- "homogène"


# Create a graph object
mygraph <- createGraph(dend, skip = T)

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, hjust = 1.6, vjust = 1, size = 6) +
  geom_node_point(aes(x = x, y=y, filter=leaf, color=type), size = 3) +
  geom_node_point(aes(x = x, y=y, filter=!leaf, color=type), size = 6) +
  # scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=17),
        legend.title=element_text(size=17)) +
  theme(legend.position = c(.215,.87)) +
  guides(color = guide_legend(override.aes = list(size=6)), alpha = F) +
  scale_colour_manual(values = pal2)

gg

ggsave(filename = "cascade1.pdf",
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
