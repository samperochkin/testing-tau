# # JUST FOR B = rep(1,p) FOR NOW
# 
# clus <- c(1,1,1,1,1,2,2,2,2,2)
# Gs <- lapply(unique(clus),function(k) which(clus==k))
# K <- length(Gs)
# 
# kk <- rbind(cbind(1:K,1:K),
#             t(combn(2,2)))
# 
# blocks <- apply(kk,1,function(k){
#   k1 <- k[1]
#   k2 <- k[2]
#   
#   l <- unique(c(l.mat[Gs[[k1]],Gs[[k2]]]))[-1]
#   cbind(l,k1,k2)
# })
# 
# B <- sapply(blocks, function(b){
#   v <- rep(0,p)
#   v[b[,1]] <- 1
#   v
# })
# 
# 
# averageSigma <- function(S, l.mat, full=F){
#   
#   d <- ncol(l.mat)
#   p <- choose(d,2)
#   
#   L <- length(blocks)
#   
#   bb <- rbind(cbind(1:L,1:L),
#                t(combn(L,2)))
#   
#   
#   for(r in 1:row(bb)){
#     
#     blocks[bb[r,1]]
#     blocks[bb[r,2]]
#     
#     G11 <- Gs[[bb[r,1]]]
#     G12 <- Gs[[bb[r,2]]]
#     
#     R <- Matrix::Matrix(0, nrow = p, ncol = length(G1), sparse = T)
#     for(i in 1:d){
#       R[l.mat[i,-i],i] <- 1
#     }
#     RtR <- Matrix::tcrossprod(R)
#     
# 
#     
#   }
#   
#   
#   B <- Matrix::Matrix(0, nrow = p, ncol = d, sparse = T)
#   for(i in 1:d){
#     B[l.mat[i,-i],i] <- 1
#   }
#   BtB <- Matrix::tcrossprod(B)
#   
#   if(!full) return(sapply(0:2, function(k) mean(S[Matrix::which(BtB == k)])))
#   
#   for(k in 0:2){
#     S[Matrix::which(BtB == k)] <- mean(S[Matrix::which(BtB == k)])
#   }
#   return(S)
# }
