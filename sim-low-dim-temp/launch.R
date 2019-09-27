# nohup R CMD BATCH launch.R log.txt &


source("simFun.R")
simFun(n = seq(100,300,100), d = seq(5,20,5), k = NULL, tau = c(.3,.6), dtau = c(0,.15,.3),
       distribution = "clayton",
       num_sim = 1000,
       filename = "clayton1",
       clus = c(rep(c("dms1","dms2", "some others look on ulaval account"),6), rep("dms11",8)))
