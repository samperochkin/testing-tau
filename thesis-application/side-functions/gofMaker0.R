M = 2000
alpha = .05

print("Setup")
library(dendextend)
sapply(list.files("thesis-application/functions/", full.names = T), source, local = environment())
source("thesis-application/setup/setup.R", local = environment())
  
Tau.hat <- cor.fk(X)
  
  
#### Initialization ####

dend <- initialObjects(Tau.hat)$dend
Tau.init <- constructTauTilde(dend, Tau.hat)
pal2 <- c("#000000", "#009E73", "#D55E00", "gold")


#### graph init ####
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
  return("indéfini")
})
type <- factor(type, levels = c("feuille","homogène","hétérogène","indéfini"))

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
  # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.75) +
  # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
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

# ggsave(filename = "arbre.pdf",
#        plot = gg,
#        device = "pdf",
#        width = 7,
#        height = 5)






#### Loop - hetero ####

vec.address <- getAddresses(dend)

non.leaf <- 0
ind <- 0

while(non.leaf < 15){
  ind <- ind + 1
  v <- rev(vec.address)[[ind]]
  
  node <- getSubDend(dend,v)
  if(is.leaf(node)){
    next
  } 
  
  non.leaf <- non.leaf + 1
  print(non.leaf)
  
  node <- correctNode(node)
  dend <- assignSubDend(node,dend,v)
  
  if(!identical(v,0)){
    attr(node, "valid") <- testOuter(node, Tau.hat, dend) > alpha
    dend <- assignSubDend(node,dend,v)
  }
  
  Tau.hetero <- constructTauTilde(dend, Tau.hat)
  
  
  vec.address2 <- getAddresses(dend)
  
  to <- sapply(vec.address2, function(v) paste0("(",paste0(v, collapse = ","),")"))
  from <- sapply(vec.address2, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
  edges <- data.table(from,to)[-1,]
  
  size <- sapply(vec.address2, function(v){
    length(unlist(getSubDend(dend,v)))
  })
  type <- sapply(vec.address2, function(v){
    node <- getSubDend(dend,v)
    if(is.leaf(node)) return("feuille") 
    if(identical(attr(node, "type"),1)) return("homogène")
    if(identical(attr(node, "type"),0)) return("hétérogène")
    attr(node, "type")
  })
  
  
  len <- (length(vec.address) - ind)
  type[1:len][type[1:len] != "feuille"] <- "indéfini"
  type <- factor(type, levels = c("feuille","homogène","hétérogène","indéfini"))
  
  tt <- constructTauTilde(dend, Tau.hat, return.single.values = T) 
  
  id1 <- sapply(vec.address2, function(v){
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
  
  maxlen <- max(sapply(vec.address2,length))
  aa <- sapply(vec.address2, function(v){
    a <- as.integer(paste0(v,collapse = ""))
    a*10^(maxlen-length(v))
  })
  vertices <- vertices[order(aa),]
  type.unique <- unique(vertices$type)
  
  # Create a graph object
  mygraph <- graph_from_data_frame( edges, vertices=vertices )
  
  
  gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
    geom_edge_diagonal(colour="black") +
    # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.75) +
    # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
    geom_node_point(aes(x = x, y=y, colour=type, filter=leaf), size = 2, alpha = 1) +
    geom_node_point(aes(x = x, y=y, colour=type, filter = !leaf), size = 5, alpha = 1) +
    theme_void() +
    theme(legend.text=element_text(size=15),
          legend.title=element_text(size=15)) +
    # theme(legend.position = c(.18,.87)) +
    guides(color = guide_legend(override.aes = list(size=5)), size = F) +
    scale_color_manual(values = c("feuille" = pal2[1], "homogène" = pal2[2],
                                  "hétérogène" = pal2[3], "indéfini" = pal2[4]),
                       limits = c("feuille","homogène","hétérogène","indéfini"))
  # theme(legend.margin = margin(c(20,0,20,0),unit = "pt")) +
  # guides(color = guide_legend(override.aes = list(size=5)))
  gg
  
  ggsave(filename = paste0("arbre-hetero",non.leaf,".pdf"),
         plot = gg,
         device = "pdf",
         width = 7,
         height = 5)
  
}

  
print("Loop - homo")
dend.final <- loopHomo(dend.hetero)
Tau.final <- constructTauTilde(dend.final, Tau.hat)
  
dends <- list(dend.init,dend.hetero,dend.final)
Taus <- list(Tau.hat,Tau.init,Tau.hetero,Tau.final)
  
return(list(dends = dends, Taus = Taus))
