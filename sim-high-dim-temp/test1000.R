library(parallel)

# clus <- makeCluster(c(rep("dms11",12),
#                       rep(paste0("dms",c(1:7,9,10)),each=6)))
clus <- makeCluster(c(rep("dms11",12),
                      rep(paste0("dms",c(1:2)),each=3),
                      rep(paste0("dms",c(3:7,9,10)),each=1)))

clusterEvalQ(clus, expr={
  library(HAC)
  library(Matrix)
  library(pcaPP)
})

source("functions/generateData.R")
source("functions/buildSigma.R")
source("functions/computeTh.R")
source("functions/computeTh4.R")
# source("functions/computeTh5.R")
clusterExport(clus, varlist=c("computeTh","computeTh3",
                              "generateData"),envir = environment())



res <- parSapply(clus, 1:500, function(dummy){
  X <- generateData(n = 100,
                    d = 500,
                    tau = .3,
                    dtau = 0,
                    distribution = "joe")
  
  n <- nrow(X)
  d <- ncol(X)
  p <- d*(d-1)/2
  Tau.hat <- pcaPP::cor.fk(X)
  
  l.ij.mat <- t(combn(d,2))
  ij.l.mat <- matrix(0,d,d)
  ij.l.mat[rbind(l.ij.mat,l.ij.mat[,2:1])] <- 1:p
  
  theta <- computeTh3(X, 10, 500)
  
  
  
  sigma <- theta - 2*(2*n - 3) / (n*(n - 1)) * (mean(Tau.hat[l.ij.mat]) + 1)^2
  sigma2 <- sigma[1]
  sigma1 <- sigma[2]
  sigma0 <- sigma[3]
  
  delta2 <- sigma2 + (d-4)*sigma1 - (d-3)*sigma0
  delta3 <- sigma2 - 2*sigma1 + sigma0
  delta <- c(delta2,delta3)
  
  B <- Matrix(0, nrow = p, ncol = d, sparse = T)
  for(i in 1:d){
    B[ij.l.mat[i,-i],i] <- 1
  }
  BtB <- tcrossprod(B)
  
  g2 <- 2/(d-1)
  g1 <- (d-3)/((d-1)*(d-2))
  g0 <- -2/((d-1)*(d-2))
  
  th <- Tau.hat[l.ij.mat]
  th.star <- as.vector(g2*th + g1*crossprod((BtB == 1), th) + g0*(sum(th) - crossprod((BtB != 0), th)))
  
  
  hist(th.star)
  
  maha <- crossprod(th-th.star)/delta3 + crossprod(th.star-mean(th))/delta2
  # c(maha,pchisq(maha, df=p-1))
  c(maha,
    pnorm(maha,p-1,sqrt(2*(p-1)),lower.tail = F))
})

saveRDS(res, "res-test.rds")



