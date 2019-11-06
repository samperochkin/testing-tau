


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
  if(length(node) == 2 | identical(attr(node, "type"),1)) return("homogène")
  if(identical(attr(node, "type"),0)) return("hétérogène")
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

tick <- names(id1)

sector <- sapply(tick, function(tick){
  if(is.na(tick)) return(NA)
  
  meta[Symbol == tick,]$Sector
})
industry <- sapply(tick, function(tick){
  if(is.na(tick)) return(NA)
  
  meta[Symbol == tick,]$industry
})

vertices <- data.table(name = to,
                       group = from,
                       id1,
                       id2,
                       tick,
                       sector,
                       industry,
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

print(type)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )









pal <- c("#000000", "#009E73", "#D55E00")

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=tick),
                 angle = -75, hjust = 0, alpha=1,
                 nudge_x = .1, nudge_y = -.2, size = 2.5) +
  # geom_node_text(aes(x = x, y=y, filter = !leaf, label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y-.045, color=type, filter=!leaf), size = 3, alpha = 1) +
  geom_node_point(aes(x = x, y=y, color=type, filter=leaf), size = 1.5, alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = c(.8,.85)) +
  # guides(color = guide_legend(override.aes = list(size=3)), size = F) +
  scale_color_manual(values = pal[c(1,3,2)],
                     guide = guide_legend(title.position = "right",
                                          title.theme = element_text(angle=-90),
                                          label.theme = element_text(angle=-90),
                                          label.position = "bottom",
                                          direction = "horizontal", reverse = T,label.hjust = 0)) +
  xlim(0,d-1+.1) + ylim(-1.25,max(sapply(vec.address,length))-1)

gg


pdf(file = "dend-flat-tickers.pdf", width = 10, height = 7)
gg
dev.off()




gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=sector),
                 angle = -75, hjust = 0, alpha=1,
                 nudge_x = .1, nudge_y = -.2, size = 2.5) +
  # geom_node_text(aes(x = x, y=y, filter = !leaf, label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y-.045, color=type, filter=!leaf), size = 3, alpha = 1) +
  geom_node_point(aes(x = x, y=y, color=type, filter=leaf), size = 1.5, alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = c(.8,.85)) +
  # guides(color = guide_legend(override.aes = list(size=3)), size = F) +
  scale_color_manual(values = pal[c(1,3,2)],
                     guide = guide_legend(title.position = "right",
                                          title.theme = element_text(angle=-90),
                                          label.theme = element_text(angle=-90),
                                          label.position = "bottom",
                                          direction = "horizontal", reverse = T,label.hjust = 0)) +
  xlim(0,d-1+.1) + ylim(-1.25,max(sapply(vec.address,length))-1)


gg
#
pdf(file = "dend-flat-sector.pdf", width = 11, height = 8.5)
gg
dev.off()





gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=industry),
                 angle = -75, hjust = 0, alpha=1,
                 nudge_x = .1, nudge_y = -.2, size = 2.5) +
  # geom_node_text(aes(x = x, y=y, filter = !leaf, label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y-.045, color=type, filter=!leaf), size = 3, alpha = 1) +
  geom_node_point(aes(x = x, y=y, color=type, filter=leaf), size = 1.5, alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = c(.8,.85)) +
  # guides(color = guide_legend(override.aes = list(size=3)), size = F) +
  scale_color_manual(values = pal[c(1,3,2)],
                     guide = guide_legend(title.position = "right",
                                          title.theme = element_text(angle=-90),
                                          label.theme = element_text(angle=-90),
                                          label.position = "bottom",
                                          direction = "horizontal", reverse = T,label.hjust = 0)) +
  xlim(0,d-1+.1) + ylim(-1.25,max(sapply(vec.address,length))-1)


gg
#
pdf(file = "dend-flat-ind.pdf", width = 11, height = 8.5)
gg
dev.off()



gg <- ggraph(mygraph, layout = 'dendrogram', circular = T)

x <- gg$data$x
y <- gg$data$y

angle <- (atan(y/x)/pi + 1/2)*180 - 90
radius <- sqrt(x^2+y^2)
hjust <- ifelse( x < 0, 1, 0)

gg$data$angle <- angle
gg$data$radius <- radius
gg$data$hjust <- hjust



sf <- .75
gg$data$x <- gg$data$x*sf
gg$data$y <- gg$data$y*sf


# gg <- ggraph(mygraph, layout = 'dendrogram', circular = T) + 
gg1 <- gg + 
  geom_edge_diagonal(colour="black") +
  #geom_node_text(aes(x = x, y=y, filter = leaf, label=plotid,
  # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1,
  # geom_node_text(aes(x = x*sf*1.05, y=y*sf*1.05, filter = leaf, label=id3,
  #                    angle=angle, hjust=hjust), alpha=1) +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=tick,
                     angle=angle, hjust=hjust), alpha=1) +
  # geom_node_text(aes(x = x, y=y, filter = leaf, label=plotid),
  #                angle=angle[is.na(vertices$id2)], hjust=hjust[is.na(vertices$id2)], alpha=1) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y, color=type, filter=!leaf), size = 3, alpha = 1) +
  geom_node_point(aes(x = x, y=y, color=type, filter=leaf), size = 1, alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.position = "none") +
  # theme(legend.text=element_text(size=15),
  #       legend.title=element_text(size=15)) +
  # theme(legend.position = c(.85,.85)) +
  guides(color = guide_legend(override.aes = list(size=5)), size = F) +
  scale_color_manual(values = pal[c(1,3,2)]) +
  xlim(-1,1) + ylim(-1,1)
# theme(legend.margin = margin(c(20,0,20,0),unit = "pt")) +
# guides(color = guide_legend(override.aes = list(size=5)))

gg1


pdf(file = "dend-circ-tick.pdf", width = 6, height = 6)
gg1
dev.off()



# 
# 
# 
# gg2 <- gg + 
#   geom_edge_diagonal(colour="black") +
#   #geom_node_text(aes(x = x, y=y, filter = leaf, label=plotid,
#   # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1,
#   # geom_node_text(aes(x = x*sf*1.05, y=y*sf*1.05, filter = leaf, label=id3,
#   #                    angle=angle, hjust=hjust), alpha=1) +
#   geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=industry,
#                      angle=angle, hjust=hjust), alpha=1) +
#   # geom_node_text(aes(x = x, y=y, filter = leaf, label=plotid),
#   #                angle=angle[is.na(vertices$id2)], hjust=hjust[is.na(vertices$id2)], alpha=1) +
#   # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
#   geom_node_point(aes(x = x, y=y, color=type, filter=!leaf), size = 3, alpha = 1) +
#   geom_node_point(aes(x = x, y=y, color=type, filter=leaf), size = 1, alpha = 1) +
#   scale_size_continuous( range = c(2,10) ) +
#   theme_void() +
#   theme(legend.position = "none") +
#   # theme(legend.text=element_text(size=15),
#   #       legend.title=element_text(size=15)) +
#   # theme(legend.position = c(.85,.85)) +
#   guides(color = guide_legend(override.aes = list(size=5)), size = F) +
#   scale_color_manual(values = pal[c(1,3,2)]) +
#   xlim(-1,1) + ylim(-1,1)
# # theme(legend.margin = margin(c(20,0,20,0),unit = "pt")) +
# # guides(color = guide_legend(override.aes = list(size=5)))
# 
# gg2
