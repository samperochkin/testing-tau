setwd("sim-low-dim")
library(data.table)
library(xtable)

# dts <- grep("joe1", list.files("joe1"), value = T)
# 
# dt <- rbindlist(lapply(seq_along(dts), function(k){
#   dt <- fread(paste0("joe1/", dts[k]))
#   dt[, decision := pval < .05]
#   dt[order(ID), .(m_decision = mean(decision)), .(n,d,tau,dtau,dtau_type,test_type)]
# }))
# 
# fwrite(dt,"joe1/agg.csv")

dt <- fread("joe1/agg.csv")
dt[, dtau_type := factor(dtau_type, levels = c("none","single","column"))]




#### table paper -- nd -- size ####
tt <- c("maha","euc","supS","sup")

dt2 <- dcast(dt[d %in% c(5,20) & dtau == 0 & test_type %in% tt & n %in% c(100,400),], test_type+n~tau+d, value.var = "m_decision")
print(xtable(dt2, digits = 3), include.rownames=FALSE)

cols <- names(dt2)[-c(1,2)]
dt2[ , (cols) := lapply(.SD, round, digits=3), .SDcols = cols]

dt3 <- matrix(NA, nrow(dt2)+4, ncol(dt2)+3)
dt3[-c(1,4,7,10),-c(3,6,9)] <- as.matrix(dt2)
colnames(dt3) <- rep(" ",ncol(dt3))
colnames(dt3)[-c(3,6,9)] <- colnames(dt2)

colnames(dt3)[grep("non",colnames(dt3))] <- gsub("none","n",colnames(dt3)[grep("non",colnames(dt3))])
colnames(dt3)[grep("col",colnames(dt3))] <- gsub("column","c",colnames(dt3)[grep("col",colnames(dt3))])
colnames(dt3)[grep("sin",colnames(dt3))] <- gsub("single","s",colnames(dt3)[grep("sin",colnames(dt3))])


print(xtable(dt3), include.rownames=FALSE)





#### table paper -- nd -- size and power ####
tt <- c("maha","euc","supS","sup")

dt2 <- dcast(dt[n %in% c(100,400) & d %in% c(5,20) & tau == .3 & dtau %in% c(0,.1) & test_type %in% tt,], test_type+n~dtau_type+d, value.var = "m_decision")
print(xtable(dt2, digits = 3), include.rownames=FALSE)

cols <- names(dt2)[-c(1,2)]
dt2[ , (cols) := lapply(.SD, round, digits=3), .SDcols = cols]

dt3 <- matrix(NA, nrow(dt2)+3, ncol(dt2)+3)
dt3[-c(3,6,9),-c(3,6,9)] <- as.matrix(dt2)
colnames(dt3) <- rep(" ",ncol(dt3))
colnames(dt3)[-c(3,6,9)] <- colnames(dt2)

colnames(dt3)[grep("non",colnames(dt3))] <- gsub("none","n",colnames(dt3)[grep("non",colnames(dt3))])
colnames(dt3)[grep("col",colnames(dt3))] <- gsub("column","c",colnames(dt3)[grep("col",colnames(dt3))])
colnames(dt3)[grep("sin",colnames(dt3))] <- gsub("single","s",colnames(dt3)[grep("sin",colnames(dt3))])


print(xtable(dt3), include.rownames=FALSE)







#### table main ####
tt <- c("maha","euc","supS","sup")

dt2 <- dcast(dt[dtau == 0 & test_type %in% tt,], n+test_type~d+tau, value.var = "m_decision")
print(xtable(dt2, digits = 3), include.rownames=FALSE)

cols <- names(dt2)[-c(1,2)]
dt2[ , (cols) := lapply(.SD, round, digits=3), .SDcols = cols]

dt3 <- matrix(NA, nrow(dt2)+4, ncol(dt2)+4)
dt3[-c(1,6,11,16),-c(3,7,11,15)] <- as.matrix(dt2)
colnames(dt3) <- rep("",ncol(dt3))
colnames(dt3)[-c(3,7,11,15)] <- colnames(dt2)

print(xtable(dt3), include.rownames=FALSE)





#### table main dtau=.1 ####
tt <- c("maha","euc","supS","sup")

dt2 <- dcast(dt[tau == .3 & dtau == .1 & test_type %in% tt,], n+test_type~d+dtau_type, value.var = "m_decision")
print(xtable(dt2, digits = 3), include.rownames=FALSE)

cols <- names(dt2)[-c(1,2)]
dt2[ , (cols) := lapply(.SD, round, digits=3), .SDcols = cols]

dt3 <- matrix(NA, nrow(dt2)+4, ncol(dt2)+4)
dt3[-c(1,6,11,16),-c(3,6,9,12)] <- as.matrix(dt2)
colnames(dt3) <- rep("",ncol(dt3))
colnames(dt3)[-c(3,6,9,12)] <- colnames(dt2)

colnames(dt3)[grep("col",colnames(dt3))] <- gsub("column","c",colnames(dt3)[grep("col",colnames(dt3))])
colnames(dt3)[grep("sin",colnames(dt3))] <- gsub("single","s",colnames(dt3)[grep("sin",colnames(dt3))])


print(xtable(dt3), include.rownames=FALSE)







#### table maha ####
tt <- c("maha","maha2","maha3")

dt2 <- dcast(dt[tau == .3 & dtau %in% c(0,.1) & test_type %in% tt & d %in% c(5,20),], n+test_type~dtau+d+dtau_type, value.var = "m_decision")
print(xtable(dt2, digits = 3), include.rownames=FALSE)

cols <- names(dt2)[-c(1,2)]
dt2[ , (cols) := lapply(.SD, round, digits=3), .SDcols = cols]

dt3 <- matrix(NA, nrow(dt2)+4, ncol(dt2)+3)
dt3[-c(1,5,9,13),-c(3,6,9)] <- as.matrix(dt2)
colnames(dt3) <- rep("",ncol(dt3))
colnames(dt3)[-c(3,6,9)] <- colnames(dt2)

colnames(dt3)[grep("non",colnames(dt3))] <- gsub("none","n",colnames(dt3)[grep("non",colnames(dt3))])
colnames(dt3)[grep("col",colnames(dt3))] <- gsub("column","c",colnames(dt3)[grep("col",colnames(dt3))])
colnames(dt3)[grep("sin",colnames(dt3))] <- gsub("single","s",colnames(dt3)[grep("sin",colnames(dt3))])


print(xtable(dt3), include.rownames=FALSE)
