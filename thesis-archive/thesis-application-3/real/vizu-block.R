true.labels <- labels(dend)
vec.address <- getAddresses(dend)

to <- sapply(vec.address, function(v) paste0("(",paste0(v, collapse = ","),")"))
from <- sapply(vec.address, function(v) paste0("(",paste0(v[-length(v)], collapse = ","),")"))
edges <- data.table(from,to)[-1,]

size <- sapply(vec.address, function(v){
  length(unlist(getSubDend(dend,v)))
})
delta <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
  return(attr(node, "delta"))
  # if(is.leaf(node)) return("feuille") 
  # if(attr(node, "delta")==1) return("homogène")
  # if(attr(node, "delta")==0) return("hétérogène")
})
delta <- factor(delta, levels = c(-1,0,1), labels = c("feuille","hétérogène","homogène"))
tt <- constructTauTilde(dend, Tau.hat, return.single.values = T)
id1 <- sapply(vec.address, function(v){
  node <- getSubDend(dend,v)
  if(is.leaf(node)) return(attr(node,"label")) 
  else(return(NA))
})

id2 <- rep(NA,length(id1))
id2[is.na(id1)] <- to[is.na(id1)]

# tick <- colnames(X)[as.numeric(id1)]
tick <- colnames(X)[as.numeric(id1)]
# tick <- id1

sector <- sapply(tick, function(tick){
  if(is.na(tick)) return(NA)
  
  meta[Symbol == tick,]$Sector
})
industry <- sapply(tick, function(tick){
  if(is.na(tick)) return(NA)
  
  meta[Symbol == tick,]$industry
})

vertices <- data.table(name = to,
                       group = from,
                       id1,
                       id2,
                       tick,
                       sector,
                       industry,
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

print(delta)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )









pal <- c("#000000", "#009E73", "#D55E00")
