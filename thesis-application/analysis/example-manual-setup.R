hc <- hclust(as.dist(1-Tau),"single")
plot(hc)

dend <- as.dendrogram(hc)

vec.address <- getAddresses(dend)
for(v in rev(vec.address)){
  node <- getSubDend(dend,v)
  node[1:length(node)] <- node[order(sapply(node,function(nn) min(unlist(nn))))]
  dend <- assignSubDend(node,dend,v)
}
vec.address <- getAddresses(dend)
plot(dend)


attr(dend[[c(1,1,1)]], "type") <- "homogène"

dend[[c(1,1,2)]] <- unbranch(dend[[c(1,1,2)]],1)
attr(dend[[c(1,1,2)]], "type") <- "homogène"

dend[[c(1,2)]] <- unbranch(dend[[c(1,2)]],1)
dend[[c(1,2)]] <- unbranch(dend[[c(1,2)]],1)
dend[[c(1,2)]] <- unbranch(dend[[c(1,2)]],1)
attr(dend[[c(1,2)]], "type") <- "homogène"

dend[[c(1)]] <- unbranch(dend[[c(1)]],1)
attr(dend[[c(1)]], "type") <- "hétérogène"

dend[[c(2,1)]] <- unbranch(dend[[c(2,1)]],1)
dend[[c(2,1)]] <- unbranch(dend[[c(2,1)]],1)
dend[[c(2,1)]] <- unbranch(dend[[c(2,1)]],4)
attr(dend[[c(2,1)]], "type") <- "hétérogène"

dend[[c(2,2)]] <- unbranch(dend[[c(2,2)]],1)
attr(dend[[c(2,2)]], "type") <- "homogène"

attr(dend[[c(1)]], "type") <- "hétérogène"
attr(dend[[c(2)]], "type") <- "homogène"
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

gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
  geom_edge_diagonal(colour="black") +
  geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.75) +
  geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
  geom_node_point(aes(x = x, y=y, colour=type, size=size), alpha = 1) +
  scale_size_continuous( range = c(2,10) ) +
  theme_void() +
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=15)) +
  # theme(legend.position = c(.18,.87)) +
  guides(color = guide_legend(override.aes = list(size=5)), size = F) +
  scale_color_manual(values = pal[rev(match(type.unique,levels(type)))])
# theme(legend.margin = margin(c(20,0,20,0),unit = "pt")) +
# guides(color = guide_legend(override.aes = list(size=5)))
gg

pdf(file = "arbre.pdf", width = 7, height = 5)
gg
dev.off()
