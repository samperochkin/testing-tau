pal2 <- c("#000000", "#009E73", "#D55E00")
dend <- struc$dends[[3]]


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

tt <- constructTauTilde(dend, struc$Taus[[1]], return.single.values = T) 

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
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.75) +
  geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y, colour=type, filter=leaf), size = 2, alpha = 1) +
  geom_node_point(aes(x = x, y=y, colour=type, filter = !leaf), size = 5, alpha = 1) +
  # scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=15)) +
  # theme(legend.position = c(.18,.87)) +
  guides(color = guide_legend(override.aes = list(size=5)), size = F) +
  scale_color_manual(values = pal2[rev(match(type.unique,levels(type)))])
# theme(legend.margin = margin(c(20,0,20,0),unit = "pt")) +
# guides(color = guide_legend(override.aes = list(size=5)))
gg

ggsave(filename = "arbre.pdf",
       plot = gg,
       device = "pdf",
       width = 7,
       height = 5)