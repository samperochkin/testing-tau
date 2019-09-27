setwd("sim-high-dim")
library(data.table)
library(xtable)

tabs <- grep("dt_high_dim_1", list.files(), value = T)
dt <- rbindlist(lapply(tabs, fread))
dt <- dt[order(ID),]
rm(tabs)

# Size --------------------------------------------------------------------
## Normal only

dt.sub <- dt[dtau == 0 & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, d + method ~ tau + n, value.var = "size")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,3), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+3, ncol(dt.sub)+3)
dt.sub2[-c(4,8,12), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)


# All others (d <= 50)

dt.sub <- dt[dtau == 0 & d <= 50 & distribution != "normal", .(size = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, distribution + d + method ~ tau + n, value.var = "size")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:3)]) := round(.SD,3), .SDcols=nn[-c(1:3)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2+3*2, ncol(dt.sub)+3)
dt.sub2[-seq(4,32,4), -c(4,8,12)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)




# power -------------------------------------------------------------------
## single
### normal only

dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "power")
dt.sub <- dcast(dt.sub, d + method ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,2), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+3, ncol(dt.sub)+3)
dt.sub2[-c(4,8,12), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)


### All others (d <= 50)

dt.sub <- dt[dtau == .1 & d <= 50 & dtau_type == "single" & distribution != "normal", .(power = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+method~n, value.var = "size")
dt.sub <- dcast(dt.sub, distribution + d + method ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:3)]) := round(.SD,2), .SDcols=nn[-c(1:3)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2+3*2, ncol(dt.sub)+3)
dt.sub2[-seq(4,32,4), -c(4,8,12)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)




## column
### normal only

dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "power")
dt.sub <- dcast(dt.sub, d + method ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,2), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+3, ncol(dt.sub)+3)
dt.sub2[-c(4,8,12), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)


### All others

dt.sub <- dt[dtau == .1 & d <= 50 & dtau_type == "column" & distribution != "normal", .(power = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, distribution + d + method ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:3)]) := round(.SD,2), .SDcols=nn[-c(1:3)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2+3*2, ncol(dt.sub)+3)
dt.sub2[-seq(4,32,4), -c(4,8,12)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)
