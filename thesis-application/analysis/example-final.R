source("thesis-application/example.R")

diag(Tau) <- 1

# for(seed in 1:5){
seed <- 5
set.seed(seed)
X <- rmvnorm(n=85, sigma = sin(pi*Tau/2))


  source("thesis-application/structureBuilder5.R")
struc <- structureBuilder(X)

##
source("thesis-application/vizu/createGraph.R")
source("thesis-application/functions/getAddresses.R")
source("thesis-application/functions/getSubDend.R")
source("thesis-application/functions/assignSubDend.R")
source("thesis-application/functions/constructTauTilde.R")

dend <- struc$dends[[1]]

vec.address <- getAddresses(dend)
for(v in rev(vec.address)){
  node <- getSubDend(dend,v)
  node[1:length(node)] <- node[order(sapply(node,function(nn) min(unlist(nn))))]
  dend <- assignSubDend(node,dend,v)
}
vec.address <- getAddresses(dend)

oo <- unlist(dend)

pal <- wes_palette("Zissou1", 110, type = "continuous")
cols <- c(rep("white",90),pal,rep("black",100))
par(mfrow = c(1,4), mar=c(1,1,1,1))
sapply(seq_along(struc[[2]]), function(k){
  Th <- struc$Taus[[k]]
  diag(Th) <- 2
  
  image(t(Th[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
})

# }



pal <- c("#000000", "#009E73", "#D55E00")
sapply(seq_along(struc[[1]]), function(k){
  
  dend <- struc$dends[[k]]
  
  vec.address <- getAddresses(dend)
  for(v in rev(vec.address)){
    node <- getSubDend(dend,v)
    node[1:length(node)] <- node[order(sapply(node,function(nn) min(which(unlist(nn) %in% oo))))]
    
    dend <- assignSubDend(node,dend,v)
  }
  vec.address <- getAddresses(dend)
  
  mygraph <- createGraph(dend)
  
  
  
  
  
  gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
    geom_edge_diagonal(colour="black") +
    # geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.5) +
    # geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
    geom_node_point(aes(x = x, y=y, color=type, size=size), alpha = 1) +
    scale_size_continuous( range = c(2,10) ) +
    theme_void() +
    theme(legend.position = "none") +
    # theme(legend.text=element_text(size=15),
    #       legend.title=element_text(size=15)) +
    # theme(legend.position = c(.18,.87)) +
    # guides(color = guide_legend(override.aes = list(size=7)), size = F) +
    scale_colour_manual(values = pal,
                        labels = c("feuille","homogène","hétérogène"),
                        limits = c("feuille","homogène","hétérogène"))
  # drop = FALSE)
  # theme(legend.margin = margin(c(20,0,20,0),unit = "pt")) +
  # guides(color = guide_legend(override.aes = list(size=5)))
  
  pdf(file = c("ex_tree_init.pdf","ex_tree_hetero.pdf","ex_tree_final.pdf")[k],5,5)
  gg
  dev.off()
})





pal <- wes_palette("Zissou1", 100, type = "continuous")
cols <- c(rep("white",90),pal,rep("black",100))


sapply(seq_along(struc[[2]]), function(k){
  
  Th <- struc$Taus[[k]]
  diag(Th) <- 2
  
  pdf(file = c("ex_Th.pdf","ex_Th_init.pdf","ex_Th_hetero.pdf","ex_Th_final.pdf")[k],5,5)
  par(mar=c(0,0,0,0))
  image(t(Th[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
  dev.off()
  
})
