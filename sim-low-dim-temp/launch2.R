# nohup R CMD BATCH launch.R log.txt &


source("simFun.R")
simFun(n = seq(100,300,100), d = seq(5,20,5), k = NULL, tau = c(.25,.5,.75), dtau = c(0,.1,.2),
       distribution = "clayton",
       num_sim = 1000,
       filename = "clayton2",
       clus = c(rep(c("dms1","dms2"),4), rep("dms11",8)))