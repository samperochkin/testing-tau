M = 2000
alpha = .05

print("Setup")
library(dendextend)
sapply(list.files("thesis-application/functions/", full.names = T), source, local = environment())
source("thesis-application/side-functions/weirdPlot.R", local = environment(), encoding = "utf8")
source("thesis-application/side-functions/correctNode2.R", local = environment())

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

ggsave(filename = "arbre-hetero0.pdf",
       plot = gg,
       device = "pdf",
       width = 7,
       height = 5)
# ggsave(filename = "arbre-0.png",
#        plot = gg,
#        device = "png",
#        width = 7,
#        height = 5)






#### Loop - hetero ####

vec.address <- getAddresses(dend)

non.leaf <- 0
ind <- 0
dend <- dendrapply(dend, function(node){
  attr(node, "type") <- 4
  node
})


while(non.leaf < 17){
  ind <- ind + 1
  v <- rev(vec.address)[[ind]]
  
  print(v)
  
  node <- getSubDend(dend,v)
  if(is.leaf(node)){
    next
  } 
  
  non.leaf <- non.leaf + 1
  print(non.leaf)
  
  node <- getSubDend(dend,v)
  node <- correctNode(node)
  
  if(!identical(v,0)){
    attr(node, "valid") <- testOuter(node, Tau.hat, dend) > alpha + .01
  }else{
    attr(node, "valid") <- TRUE
  }
  dend <- assignSubDend(node,dend,v)
  
  gg <- weirdPlot(dend,non.leaf)
  ggsave(filename = paste0("arbre",non.leaf,".pdf"),
         plot = gg,
         device = "pdf",
         width = 7,
         height = 5)
}


#### Loop - homo ####

vec.address <- getAddresses(dend)
ind <- 0

dend <- dendrapply(dend, function(node){
  if(length(node) == 2) attr(node,"type") <- 1
  node
})


while(non.leaf < 23){
  ind <- ind + 1
  v <- rev(vec.address)[[ind]]
  print(v)
  
  # upload node
  node <- getSubDend(dend,v)
  
  # trivial cases
  if(attr(node,"members") <= 2) next
  if(attr(node,"type") == 0) next
  
  while(T){
    
    # children that could be pulled
    candidates <- which(!sapply(node, is.leaf))
    candidates <- candidates[which(sapply(node[candidates], attr, which="type")==1)]
    
    if(length(candidates) == 0) break
    
    # select the cluster formed last
    k <- candidates[which.max(get_childrens_heights(node)[candidates])]
    
    # select the cluster formed last
    pval <- testInner(Tau.hat,node,k)
    
    if(pval > .1){ #### CHEAT
      node <- unbranch(node,k)
      attr(node,"type") <- 1
      
      non.leaf <- non.leaf + 1
      print(non.leaf)
      gg <- weirdPlot2(node,dend,v,non.leaf)
      ggsave(filename = paste0("arbre-hetero",non.leaf,".pdf"),
             plot = gg,
             device = "pdf",
             width = 7,
             height = 5)
    }else{
      break
    }
  }
  
  dend <- assignSubDend(node,dend,v)
}


