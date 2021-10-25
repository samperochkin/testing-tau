library(ggraph)
library(igraph)

d <- 18
pal2 <- c("#000000", "#009E73", "#D55E00", "gold", "mediumorchid", "mediumturquoise")


source("thesis-application-3/functions/getAddresses.R")
source("thesis-application-3/functions/getSubDend.R")
source("thesis-application-3/functions/assignSubDend.R")
source("thesis-application-3/side-functions/changeDelta.R")
source("thesis-application-3/functions/constructTauTilde.R")


count <- 0
for(s in seq_along(struc$dend.begin.list)){
  v <- struc$v.list[[s]]
  dend <- struc$dend.begin.list[[s]]
  res <- struc$res.vec[s]
  
  if(is.leaf(getSubDend(dend,v))) next
  
  count <- count + 1
  source("thesis-application-3/example/gifGraphSource.R", encoding = "UTF-8")
  ggsave(filename = paste0("arbre-",count,".pdf"),
         plot = gg,
         device = "pdf",
         width = 7,
         height = 5)

  count <- count + 1
  dend <- changeDelta(dend,v,-3)
  source("thesis-application-3/example/gifGraphSource.R", encoding = "UTF-8")
  ggsave(filename = paste0("arbre-",count,".pdf"),
         plot = gg,
         device = "pdf",
         width = 7,
         height = 5)
  
  dends <- struc$dend.modifs.list[[s]]
  for(r in seq_along(dends)){
    w <- c(v,struc$v.modifs.list[[s]][r])
    dend <- struc$dend.modifs.list[[s]][[r]]
    res <- struc$res.modifs.list[[s]][r]
    
    count <- count + 1
    source("thesis-application-3/example/gifGraphSource.R", encoding = "UTF-8")
    ggsave(filename = paste0("arbre-",count,".pdf"),
           plot = gg,
           device = "pdf",
           width = 7,
           height = 5)
    
    count <- count + 1
    dend <- changeDelta(dend,w,-4)
    source("thesis-application-3/example/gifGraphSource.R", encoding = "UTF-8")
    ggsave(filename = paste0("arbre-",count,".pdf"),
           plot = gg,
           device = "pdf",
           width = 7,
           height = 5)
    
  }
  
}


count <- count+1
dend <- struc$dend
source("thesis-application-3/example/gifGraphSource.R", encoding = "UTF-8")
ggsave(filename = paste0("arbre-",count,".pdf"),
       plot = gg,
       device = "pdf",
       width = 7,
       height = 5)
