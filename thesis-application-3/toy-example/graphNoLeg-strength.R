#### graph init ####
vec.address <- getAddresses(dend)

to <- sapply(vec.address, function(v) paste0("(",paste0(v, collapse = ","),")"))
from <- sapply(vec.address, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address, function(v){
  length(unlist(getSubDend(dend,v)))
})
delta <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
  if(is.leaf(node)) return("feuille") 
  if(attr(node,"delta") == -3) return("validation") 
  if(attr(node,"delta") == -4) return("simplification") 
  if(attr(node,"delta") == 1) return("homogène") 
  if(attr(node,"delta") == 0) return("hétérogène") 
  return("indéfini")
})
delta <- factor(delta, levels = c("feuille","homogène","hétérogène","indéfini","validation","simplification"))

tt <- constructTauTilde(dend, Tau.hat, return.single.values = T) 

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
                       delta=delta)

maxlen <- max(sapply(vec.address,length))
aa <- sapply(vec.address, function(v){
  a <- as.integer(paste0(v,collapse = ""))
  a*10^(maxlen-length(v))
})
vertices <- vertices[order(aa),]
delta.unique <- unique(vertices$delta)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black", strength = stren) +
  # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.75) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y, colour=delta, filter=leaf), size = 2, alpha = 1) +
  geom_node_point(aes(x = x, y=y, colour=delta, filter = !leaf), size = 5, alpha = 1) +
  theme_void() +
  # theme(legend.text=element_text(size=15),
  #       legend.title=element_text(size=15)) +
  theme(legend.position = "none") +
  guides(color = guide_legend(override.aes = list(size=5)), size = F) +
  scale_color_manual(values = c("feuille" = pal2[1], "homogène" = pal2[2],
                                "hétérogène" = pal2[3], "indéfini" = pal2[4],
                                "validation" = pal2[5], "simplification" = pal2[6]),
                     limits = c("feuille","homogène","hétérogène","indéfini","validation","simplification"))
