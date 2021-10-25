# weirdPlot <- function(node,dend,v,non.leaf){
weirdPlot <- function(dend,non.leaf,leg = T){
  
  vec.address2 <- getAddresses(dend)
  for(w in rev(vec.address2)){
    
    node <- getSubDend(dend,w)
    node <- correctNode2(node)
    dend <- assignSubDend(node,dend,w)
    
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
    if(identical(attr(node, "valid"),FALSE)) return("indéfini")
    if(identical(attr(node, "valid"),TRUE) & identical(attr(node, "type"),0)) return("hétérogène")
    if(identical(attr(node, "valid"),TRUE) | identical(attr(node, "type"),1)) return("homogène")
    # attr(node, "type")
    return("indéfini")
  })
  
  
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
    scale_color_manual(values = c("feuille" = pal2[1], "homogène" = pal2[2],
                                "hétérogène" = pal2[3], "indéfini" = pal2[4]),
                     limits = c("feuille","homogène","hétérogène","indéfini"))
  
    if(leg){
      gg <- gg + theme(legend.text=element_text(size=15),
                       legend.title=element_text(size=15))
      # theme(legend.position = c(.18,.87)) +
      # theme(legend.margin = margin(c(20,0,20,0),unit = "pt")) +
      # guides(color = guide_legend(override.aes = list(size=5)))
    }else{
      gg <- gg + theme(legend.position = "none") 
    }  
    
  gg
  
}
