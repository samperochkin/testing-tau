# Compute Monte Carlo quantile

mc_samp <- lapply(seq(5,20,5), function(d){
  p <- d*(d-1)/2
  cbind(rchisq(10000,p-d),rchisq(10000,d-1))
})
names(mc_samp) <- seq(5,20,5)

monteCarlo <- function(res,r){
  loss_type <- res[r,loss_type]
  loss <- res[r,loss]
  n <- res[r,n]
  d <- res[r,d]
  p <- res[r,p]
  delta2 <- n*res[r,delta2]
  delta3 <- n*res[r,delta3]
  
  
  if(loss_type == "euc"){
    mc_samp_temp <- mc_samp[[as.character(d)]] %*% c(delta3,delta2)
    return(mean(mc_samp_temp > loss))
  }else if(loss_type == "euc2"){
    mc_samp_temp <- mc_samp[[as.character(d)]][,2] * delta2
    return(mean(mc_samp_temp > loss))
  }else if(loss_type == "euc3"){
    mc_samp_temp <- mc_samp[[as.character(d)]][,1] * delta3
    return(mean(mc_samp_temp > loss))
  }
  
}

# Too much overhead
# clus <- makeCluster(detectCores()-1)
# clusterEvalQ(clus, expr={
#   library(data.table)
#   library(Matrix)
# })
# clusterExport(clus, varlist=c("monteCarlo","res","mc_samp"), envir = environment())


ind <- grep("euc",res$loss_type)
# res[ind, pval := parSapply(clus,ind, function(r) monteCarlo(res,r))]
res[ind, pval := sapply(ind, function(r) monteCarlo(res,r))]
