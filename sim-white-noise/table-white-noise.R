setwd("sim-white-noise")
library(data.table)
library(xtable)

dt <- fread("dt_white_noise.csv")
dt <- dt[order(ID),]

# Size --------------------------------------------------------------------
## Normal only

dt.sub <- dt[distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, method, n, d)]
# dcast(dt.sub, tau+test_type~n, value.var = "size")
dt.sub <- dcast(dt.sub, d + method ~ tau + n, value.var = "size")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:2)]) := round(.SD,3), .SDcols=nn[-c(1:2)]]

dt.sub2 <- matrix(NA, nrow(dt.sub), ncol(dt.sub)+2)
dt.sub2[, -c(3,7)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(3,7)] <- nn
colnames(dt.sub2) <- nn2

print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)
