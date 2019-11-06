createGraph <- function(dend){
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
  
  print(type)
  
  # Create a graph object
  graph_from_data_frame( edges, vertices=vertices )
}
