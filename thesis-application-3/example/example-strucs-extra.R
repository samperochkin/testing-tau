source("thesis-application-3/functions/validify.R")

k <- 31
dend <- struc$dend.begin.list[[k]]
v <- struc$v.list[[k]]
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
gg
ggsave(filename = paste0("ex_arbre_",k,"_pre.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)
