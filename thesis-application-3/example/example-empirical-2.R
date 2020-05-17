library(mvtnorm)
library(pcaPP)
library(data.table)
library(ggraph)
library(igraph)
library(dendextend)

source("thesis-application/example/example-matrices.R")


diag(Tau) <- 1
# pal <- wes_palette("Zissou1", 112, type = "continuous")
# cols <- c(rep("white",88),pal,rep("black",100))


seed <- 12
set.seed(seed)
X <- rmvnorm(n=85, sigma = sin(pi*Tau/2))
# X <- rmvnorm(n=180, sigma = Tau)


image(cor.fk(X))

# source("thesis-application-3/structureBuilder.R")
source("thesis-application-3/example/structureBuilderTrail.R")
struc <- structureBuilderTrail(X, hclust_method = "mcquitty", M=2000,alpha=.05)
plot(struc$dend)

par(mfrow=c(1,2), mar = c(1,1,1,1))
image(t(cor.fk(X)[18:1,]), zlim = c(-.2,1))
image(t(struc$Tau.tilde[18:1,]), zlim = c(-.2,1))
par(mfrow=c(1,1), mar = c(3,3,1,1))

# source("thesis-application/structureBuilder5.R")
# struc <- structureBuilder(X)
# plot(struc$dends[[3]])

##

Tau.hat <- cor.fk(X)
Th <- Tau.hat
diag(Th) <- 2

pal <- wes_palette("Zissou1", 120, type = "continuous")
cols <- c(rep("white",80),pal,rep("black",100))

oo <- unlist(struc$dend)

pdf(file = c("ex_Th.pdf"),5,5)
par(mar=c(0,0,0,0))
image(t(Th[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

Th0 <- struc$Tau.list[[1]]
diag(Th0) <- 2

pdf(file = c("ex_Th_init.pdf"),5,5)
par(mar=c(0,0,0,0))
image(t(Th0[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

Tt <- struc$Tau.tilde
diag(Tt) <- 2

pdf(file = c("ex_Tt.pdf"),5,5)
par(mar=c(0,0,0,0))
image(t(Tt[rev(oo),oo]), col = cols, zlim = c(-1,2), axes = F)
dev.off()

source("thesis-application-3/functions/getAddresses.R")
source("thesis-application-3/functions/getSubDend.R")
source("thesis-application-3/functions/assignSubDend.R")
source("thesis-application-3/side-functions/changeDelta.R")
source("thesis-application-3/functions/constructTauTilde.R")
pal2 <- c("#000000", "#009E73", "#D55E00", "gold", "mediumorchid", "mediumturquoise")

dend <- struc$dend.begin.list[[1]]
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
ggsave(filename = paste0("ex_arbre_init.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)

attr(dend,"delta") <- 1
attr(dend[[1]],"delta") <- 0
attr(dend[[c(2,1)]],"delta") <- 0
attr(dend[[c(2,2)]],"delta") <- 1

attr(dend[[c(1,1)]],"delta") <- 1
attr(dend[[c(1,2,1)]],"delta") <- 1
attr(dend[[c(1,2,2)]],"delta") <- 1

source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
ggsave(filename = paste0("ex_arbre_init2.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)

dend <- struc$dend
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")
ggsave(filename = paste0("ex_arbre.pdf"),
       plot = gg,
       device = "pdf",
       width = 5,
       height = 5)


dend <- struc$dend
labels(dend)
labels(dend) <- 1:18
source("thesis-application-3/example/graphNoLeg.R", encoding = "UTF-8")


gg <- ggraph(mygraph, layout = 'dendrogram', circular = F) + 
        geom_edge_diagonal(colour="black") +
        geom_node_text(aes(x = x, y=y, filter = leaf, label=id1), alpha=1, vjust = 1.75) +
        geom_node_text(aes(x = x, y=y, filter = !is.na(id2), label=id2), alpha=1,nudge_x = T, angle = 25) +
        geom_node_point(aes(x = x, y=y, colour=delta, filter=leaf), size = 2, alpha = 1) +
        geom_node_point(aes(x = x, y=y, colour=delta, filter = !leaf), size = 5, alpha = 1) +
        theme_void() +
        # theme(legend.text=element_text(size=15),
        #       legend.title=element_text(size=15)) +
        theme(legend.position = "none") +
        guides(color = guide_legend(override.aes = list(size=5)), size = F) +
        scale_color_manual(values = c("feuille" = pal2[1], "homogène" = pal2[2],
                                      "hétérogène" = pal2[3], "indéfini" = pal2[4],
                                      "validation" = pal2[5], "simplification" = pal2[6]),
                           limits = c("feuille","homogène","hétérogène")) +
        theme_void() +
        theme(legend.text=element_text(size=15),
              legend.title=element_text(size=15)) +
        # theme(legend.position = c(.18,.87)) +
        theme(legend.position = "left") +
        guides(color = guide_legend(override.aes = list(size=5)), size = F)

gg
ggsave(filename = "arbre.pdf",
       plot = gg,
       device = "pdf",
       width = 7,
       height = 5)


