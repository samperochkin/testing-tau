setwd("sim-extra")
library(data.table)
library(xtable)
dt100 <- fread("dt_extra_1.csv")
dt200 <- fread("dt_extra_2.csv")

dt100[,n:=100]
dt200[,n:=200]

dt <- rbind(dt100,dt200)

# Size --------------------------------------------------------------------

dt <- melt(dt,id.vars = c("cor","n"),variable.name = "test",value.name = "pval")

dt.sub <- dt[, .(size = mean(pval < .05)), .(cor,test,n)]
dt.sub <- dcast(dt.sub, test + n ~ cor, value.var = "size")
dt.sub


# power -------------------------------------------------------------------
## single

dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")
dt.sub

## column

dt.sub <- dt[dtau == .1 & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")
dt.sub






main_tests <- c("euc","maha","sup","sups","sup_boot")

# Size --------------------------------------------------------------------

dt.sub <- dt[dtau == 0 & test %in% main_tests & sigma != "weird" & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "size")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:3)]) := round(.SD,3), .SDcols=nn[-c(1:3)]]

dt.sub2 <- matrix(NA, nrow(dt.sub) + length(unique(dt.sub$d))*3, ncol(dt.sub)+3)
dt.sub2[-c(seq(2,nrow(dt.sub2),12),
           seq(7,nrow(dt.sub2),12),
           seq(12,nrow(dt.sub2),12)),
        -c(4,8,12)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub2) <- nn2
print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)


# power -------------------------------------------------------------------
## single

dt.sub <- dt[dtau == .1 & test %in% main_tests & sigma != "weird" & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:3)]) := round(.SD,3), .SDcols=nn[-c(1:3)]]

dt.sub2 <- matrix(NA, nrow(dt.sub) + length(unique(dt.sub$d))*3, ncol(dt.sub)+3)
dt.sub2[-c(seq(2,nrow(dt.sub2),12),
           seq(7,nrow(dt.sub2),12),
           seq(12,nrow(dt.sub2),12)),
        -c(4,8,12)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub2) <- nn2
print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)

## column

dt.sub <- dt[dtau == .1 & test %in% main_tests & sigma != "weird" & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:3)]) := round(.SD,3), .SDcols=nn[-c(1:3)]]

dt.sub2 <- matrix(NA, nrow(dt.sub) + length(unique(dt.sub$d))*3, ncol(dt.sub)+3)
dt.sub2[-c(seq(2,nrow(dt.sub2),12),
           seq(7,nrow(dt.sub2),12),
           seq(12,nrow(dt.sub2),12)),
        -c(4,8,12)] <- as.matrix(dt.sub)

nn2 <- rep(" ",ncol(dt.sub2))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub2) <- nn2
print(xtable(dt.sub2, digits = 3), include.rownames=FALSE)
