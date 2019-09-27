library(ggplot2)

res <- readRDS("sim-sigma/res50.rds")
t1 <- lapply(1:3, function(i){
  res2 <- sapply(res, function(rr){
    rr[i,]
  })
  # par(mfrow=c(2,4))
  # apply(res2-1,1,hist,breaks=10,xlim=range(res2-1))
  
  mat <- rbind(apply(res2-1,1,mean),
               apply(res2,1,var),
               apply(res2-1,1,crossprod)/(ncol(res2)-1))
  mat[,-c(3,7)]
})

M <- t(sapply(t1, function(tt) tt[1,]))
V <- t(sapply(t1, function(tt) tt[2,]))
MSE <- t(sapply(t1, function(tt) tt[3,]))



res <- readRDS("sim-sigma/res100.rds")
t2 <- lapply(1:3, function(i){
  res2 <- sapply(res, function(rr){
    rr[i,]
  })
  par(mfrow=c(2,4))
  apply(res2-1,1,hist,breaks=10,xlim=range(res2-1))
  
  mat <- rbind(apply(res2-1,1,mean),
               apply(res2,1,var),
               apply(res2-1,1,crossprod)/(ncol(res2)-1))
  mat[,-c(3,7)]
})


M <- t(sapply(t2, function(tt) tt[1,]))
SE <- t(sapply(t2, function(tt) sqrt(tt[2,])))
sMSE <- t(sapply(t2, function(tt) sqrt(tt[3,])))

nn <- rep(colnames(M),times=3,each=3)
nn1 <- sapply(nn, function(nnn) substr(nnn,1,2)[[1]])
nn2 <- sapply(nn, function(nnn) substr(nnn,3,10)[[1]])

dat <- data.table(Estimator = nn2,
                  Quantity = as.factor(nn1),
                  d = rep(c(5,10,25), times=3*3),
                  Type = factor(rep(c("Bias","SE","sqrt(MSE)"),each=18), levels=c("Bias","SE","sqrt(MSE)")),
                  Value = c(M,SE,sMSE))

ggplot(dat, aes(y=Value, x=d, col=Estimator, Type)) + 
  geom_line() +
  facet_grid(Quantity~Type,
            scales = "free",
            labeller = labeller(Quantity=c(s1="sigma10",s2="sigma210"))) +
  scale_color_discrete(labels = c(JK="jackknife", PI2="plugin", L2="leave-2-out"))





res <- readRDS("sim-sigma/res150.rds")
lapply(1:3, function(i){
  res2 <- sapply(res, function(rr){
    rr[i,]
  })
  par(mfrow=c(2,4))
  apply(res2-1,1,hist,breaks=10,xlim=range(res2-1))
  
  mat <- rbind(apply(res2-1,1,mean),
               apply(res2,1,var),
               apply(res2-1,1,crossprod)/(ncol(res2)-1))
  mat[,-c(3,7)]
})



library(data.table)
do.call("rbind",lapply(c(50,100,150), function(nn){
  res <- readRDS(paste0("sim-sigma/res",nn,".rds"))
  do.call("rbind",
            lapply(1:3, function(i){
              res2 <- sapply(res, function(rr){
                rr[i,]
              })
              mat <- rbind(apply(res2-1,1,mean),
                           apply(res2,1,var),
                           apply(res2-1,1,crossprod)/(ncol(res2)-1))
              mat[,-c(3,7)]
            })
  )  
}))
