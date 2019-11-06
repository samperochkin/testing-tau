dend[[c(1,2)]] <- unbranch(dend[[c(1,2)]],1)
attr(dend[[c(1,2)]], "type") <- "homogène"

dend[[c(1,3)]] <- unbranch(dend[[c(1,3)]],1)
dend[[c(1,3)]] <- unbranch(dend[[c(1,3)]],1)
dend[[c(1,3)]] <- unbranch(dend[[c(1,3)]],1)
attr(dend[[c(1,3)]], "type") <- "homogène"

dend[[c(2,2)]] <- unbranch(dend[[c(2,2)]],1)
attr(dend[[c(2,2)]], "type") <- "homogène"


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

pal <- c("#000000", "#009E73", "#D55E00")
# c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.5) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y, colour=type, size=size), alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  # theme(legend.position = "none") +
  theme(legend.text=element_text(size=20,margin = margin(t = 5, b = 5)),
        legend.title=element_text(size=20),
        legend.spacing.y = unit(15,"pt")) +
  # theme(legend.position = c(.9,.85)) +
  guides(color = guide_legend(override.aes = list(size=5)), size = F) +
  scale_color_manual(values = pal[rev(match(type.unique,levels(type)))])
# theme(legend.margin = margin(c(20,0,20,0),unit = "pt")) +
# guides(color = guide_legend(override.aes = list(size=5)))

gg

pdf(file = "arbre3.pdf", width = 7.5, height = 5)
gg
dev.off()
