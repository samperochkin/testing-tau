setwd("sim-low-dim")
library(data.table)
library(xtable)
dt <- fread("dt_low_dim.csv")
dt <- dt[order(ID),]

dtmaha <- dt[grepl("maha2",test_type)|grepl("maha3",test_type),.(test_type = "maha23", pval = as.numeric(all(sort(pval) > c(.025,.05)))),.(n,d,tau,dtau,dtau_type,distribution,ID,num_sim)]
dtsupS <- dt[grepl("supS2",test_type)|grepl("supS3",test_type),.(test_type = "supS23", pval = as.numeric(all(sort(pval) > c(.025,.05)))),.(n,d,tau,dtau,dtau_type,distribution,ID,num_sim)]

# dtnew <- rbindlist(list(dt,dtmaha), fill=T)
dtnew <- rbindlist(list(dt,dtsupS,dtmaha), fill=T)
# main.types <- c("maha", "euc", "supS", "sup")
# main.types <- c("maha", "maha23", "maha2", "maha3")
# main.types <- c("supS", "supS23", "supS2", "supS3")
# main.types <- c("supS", "supS23", "supS2", "supS3","maha", "maha23", "maha2", "maha3")
main.types <- c("supS23","maha23")

lev <- character(length(unique(dt$test_type)) + 2)
lev[-c(2,9)] <- unique(dt$test_type)
lev[c(2,9)] <- c("maha23","supS23")

dtnew$test_type <- factor(dtnew$test_type, levels = lev)
# Size --------------------------------------------------------------------
## Normal only

dt.sub <- dtnew[dtau == 0 & test_type %in% main.types & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, d + test_type ~ tau + n, value.var = "size")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,3), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2, ncol(dt.sub)+3)
dt.sub2[-c(5,10), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)


# All others

# dt.sub <- dt[dtau == 0 & test_type %in% main.types & distribution != "normal", .(size = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# # dcast(dt.sub, tau+test_type~n, value.var = "size")
# dt.sub <- dcast(dt.sub, distribution + d + test_type ~ tau + n, value.var = "size")
# 
# nn <- names(dt.sub)
# dt.sub[,(nn[-c(1:3)]) := round(.SD,3), .SDcols=nn[-c(1:3)]]
# 
# dt.sub2 <- matrix(NA, nrow(dt.sub)+2+3*2, ncol(dt.sub)+3)
# dt.sub2[-seq(5,40,5), -c(4,8,12)] <- as.matrix(dt.sub)
# 
# nn2 <- rep(" ",ncol(dt.sub2))
# nn2[-c(4,8,12)] <- nn
# colnames(dt.sub2) <- nn2
# 
# print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)
# 



# power -------------------------------------------------------------------
## single
### normal only

dt.sub <- dtnew[dtau == .1 & test_type %in% main.types & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "power")
dt.sub <- dcast(dt.sub, d + test_type ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,2), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2, ncol(dt.sub)+3)
dt.sub2[-c(5,10), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)


### All others

# dt.sub <- dt[dtau == .1 & dtau_type == "single" & test_type %in% main.types & distribution != "normal", .(power = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# # dcast(dt.sub, tau+test_type~n, value.var = "size")
# dt.sub <- dcast(dt.sub, distribution + d + test_type ~ tau + n, value.var = "power")
# 
# nn <- names(dt.sub)
# dt.sub[,(nn[-c(1:3)]) := round(.SD,2), .SDcols=nn[-c(1:3)]]
# 
# dt.sub2 <- matrix(NA, nrow(dt.sub)+2+3*2, ncol(dt.sub)+3)
# dt.sub2[-seq(5,40,5), -c(4,8,12)] <- as.matrix(dt.sub)
# 
# nn2 <- rep(" ",ncol(dt.sub2))
# nn2[-c(4,8,12)] <- nn
# colnames(dt.sub2) <- nn2
# 
# print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)




## column
### normal only

dt.sub <- dtnew[dtau == .1 & test_type %in% main.types & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "power")
dt.sub <- dcast(dt.sub, d + test_type ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,2), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2, ncol(dt.sub)+3)
dt.sub2[-c(5,10), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)


### All others
# 
# dt.sub <- dt[dtau == .1 & test_type %in% main.types & dtau_type == "column" & test_type %in% main.types & distribution != "normal", .(power = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# # dcast(dt.sub, tau+test_type~n, value.var = "size")
# dt.sub <- dcast(dt.sub, distribution + d + test_type ~ tau + n, value.var = "power")
# 
# nn <- names(dt.sub)
# dt.sub[,(nn[-c(1:3)]) := round(.SD,2), .SDcols=nn[-c(1:3)]]
# 
# dt.sub2 <- matrix(NA, nrow(dt.sub)+2+3*2, ncol(dt.sub)+3)
# dt.sub2[-c(4,8), -c(4,8,12)] <- as.matrix(dt.sub)
# 
# nn2 <- rep(" ",ncol(dt.sub2))
# nn2[-c(4,8,12)] <- nn
# colnames(dt.sub2) <- nn2
# 
# print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)
