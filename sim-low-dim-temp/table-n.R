library(data.table)
library(xtable)
res <- fread("res-pval.csv")


res[, decision := pval < .05]

res <- res[order(ID),]
res2 <- res[, .(m_decision = mean(decision)), .(n,d,tau,dtau,loss_type)]


types <- paste0("maha", c("","2","3"))
res2 <- res[dtau == 0 & tau == .25 & loss_type %in% types, .(m_decision = mean(decision)), .(n,d,loss_type)]
dcast(res2, n + d ~ loss_type, value.var = "m_decision")

types <- paste0("maha", c("","2","3"))
res2 <- res[dtau == .1 & tau == .25 & loss_type %in% types, .(m_decision = mean(decision)), .(n,d,loss_type)]
dcast(res2, n + d ~ loss_type, value.var = "m_decision")


types <- paste0("euc", c("","2","3"))
res2 <- res[dtau == 0 & tau == .25 & loss_type %in% types, .(m_decision = mean(decision)), .(n,d,loss_type)]
dcast(res2, n + d ~ loss_type, value.var = "m_decision")

types <- paste0("supS", c("","2","3"))
res2 <- res[dtau == 0 & tau == .25 & loss_type %in% types, .(m_decision = mean(decision)), .(n,d,loss_type)]
dcast(res2, n + d ~ loss_type, value.var = "m_decision")

types <- paste0("sup", c("","2","3"))
res2 <- res[dtau == 0 & tau == .25 & loss_type %in% types, .(m_decision = mean(decision)), .(n,d,loss_type)]
dcast(res2, n + d ~ loss_type, value.var = "m_decision")


res2 <- res[, .(m_decision = mean(decision)), .(n,d,tau,dtau,loss_type)]
res3 <- dcast(res2, ...~loss_type, value.var = "m_decision")
res3 <- res3[d==10 & dtau ==0, -c(2,4)][order(tau),c(2,1,3:14)]

mat <- matrix(NA,11,18)
mat[c(1:3,5:7,9:11),c(1,2,4:6,8:10,12:14,16:18)] <- as.matrix(res3)
colnames(mat)[c(1,2,4:6,8:10,12:14,16:18)] <- colnames(res3)

print(xtable(mat), include.rownames=FALSE)





res2 <- res[loss_type %in% c("maha","euc","supS","sup"), .(m_decision = mean(decision)), .(n,d,tau,dtau,loss_type)]
res3 <- res2[d==10 & dtau ==0, -c(2,4)][order(tau)]
res3 <- dcast(res3, loss_type~tau+n, value.var = "m_decision")

print(xtable(res3, digits = 3), include.rownames=FALSE)



res3 <- res2[d==10 & dtau ==.1, -c(2,4)][order(tau)]
# res3 <- dcast(res3, alt_type+loss_type~tau+n, value.var = "m_decision")
res3 <- as.matrix(dcast(res3, loss_type~tau+n, value.var = "m_decision"))

print(xtable(as.matrix(res3), digits = 3), include.rownames=FALSE)
