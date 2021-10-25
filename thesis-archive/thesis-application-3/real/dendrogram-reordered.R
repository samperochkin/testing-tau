library(ggraph)
library(igraph)

dend <- struc$dend
plot(dend)

# RE-ORDERING
subdend <- dend
newO <- c(2,1,3)
for(k in seq_along(subdend)){
  dend[[k]] <- subdend[[newO[k]]]
}


subdend <- dend[[3]]
sapply(subdend, max_depth)
newO <- c(3,5,6,2,4,8,1,7)
for(k in seq_along(subdend)){
  dend[[c(3,k)]] <- subdend[[newO[k]]]
}



subdend <- dend[[c(3,8,2)]]
sapply(subdend, max_depth)
newO <- c(4,3,1,2)
for(k in seq_along(subdend)){
  dend[[c(3,8,2,k)]] <- subdend[[newO[k]]]
}


subdend <- dend[[c(3,8,2,1)]]
sapply(subdend, max_depth)
newO <- rev(seq_along(subdend))
for(k in seq_along(subdend)){
  dend[[c(3,8,2,1,k)]] <- subdend[[newO[k]]]
}


subdend <- dend[[c(3,6,3)]]
sapply(subdend, max_depth)
newO <- order(sapply(subdend, max_depth))
for(k in seq_along(subdend)){
  dend[[c(3,6,3,k)]] <- subdend[[newO[k]]]
}


subdend <- dend[[c(3,8,2,4,4,2)]]
sapply(subdend, max_depth)
newO <- order(sapply(subdend, max_depth))
for(k in seq_along(subdend)){
  dend[[c(3,8,2,4,4,2,k)]] <- subdend[[newO[k]]]
}



Tau.hat <- pcaPP::cor.fk(X)
d <- ncol(X)

invisible(sapply(list.files("thesis-application-3/functions/", full.names = T), source, local = environment()))

source("thesis-application-3/real/vizu-block.R")

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black",strength = .8) +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=tick),
                 angle = -75, hjust = 0, alpha=1,
                 nudge_x = .1, nudge_y = -.2, size = 2.5) +
  # geom_node_text(aes(x = x, y=y, filter = !leaf, label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y-.045, color=delta, filter=!leaf), size = 3, alpha = 1) +
  geom_node_point(aes(x = x, y=y, color=delta, filter=leaf), size = 1.5, alpha = 1) +
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
