setwd("sim-low-dim")
library(data.table)
library(xtable)
dt <- fread("dt_low_dim_un.csv")
dt <- dt[order(ID),]

main.types <- c("maha", "euc", "supS", "sup")



# Size --------------------------------------------------------------------
## Normal only

dt.sub <- dt[dtau == 0 & test_type %in% main.types & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, d + test_type ~ tau + n, value.var = "size")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,3), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2, ncol(dt.sub)+3)
dt.sub2[-c(3,6), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)



# power -------------------------------------------------------------------
## single
### normal only

dt.sub <- dt[dtau == .1 & test_type %in% main.types & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "power")
dt.sub <- dcast(dt.sub, d + test_type ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,2), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2, ncol(dt.sub)+3)
dt.sub2[-c(3,6), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)




## column
### normal only

dt.sub <- dt[dtau == .1 & test_type %in% main.types & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, test_type, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "power")
dt.sub <- dcast(dt.sub, d + test_type ~ tau + n, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,2), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub)+2, ncol(dt.sub)+3)
dt.sub2[-c(3,6), -c(3,7,11)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7,11)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)


