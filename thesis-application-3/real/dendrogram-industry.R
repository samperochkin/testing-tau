gg <- ggraph(mygraph, layout = 'dendrogram', circular = F)
tick_ind <- sapply(1:nrow(gg$data), function(r){
  paste(gg$data[r,"industry"],gg$data[r,"tick"], sep = " - ")
})
gg$data$"tick_ind" <- tick_ind

gg <- gg + 
  geom_edge_diagonal(colour="black",strength = .8) +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=tick_ind),
                 angle = -90, hjust = 0, alpha=1,
                 nudge_x = .1, nudge_y = -.2, size = 2.5) +
  # geom_node_text(aes(x = x, y=y, filter = !leaf, label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y-.045, color=delta, filter=!leaf), size = 3, alpha = 1) +
  geom_node_point(aes(x = x, y=y, color=delta, filter=leaf), size = 1.5, alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = c(.85,.85)) +
  # guides(color = guide_legend(override.aes = list(size=3)), size = F) +
  scale_color_manual(values = pal[c(1,3,2)],
                     guide = guide_legend(title.position = "right",
                                          title.theme = element_text(angle=-90),
                                          label.theme = element_text(angle=-90),
                                          label.position = "bottom",
                                          direction = "horizontal", reverse = T,label.hjust = 0)) +
  xlim(0,d-1+.1) + ylim(-5,max(sapply(vec.address,length))-1)


gg


