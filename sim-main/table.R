setwd("sim-main")
library(data.table)
library(xtable)
dt <- rbindlist(list(fread("dt_main_low_1.csv"),fread("dt_main_low_2.csv"),fread("dt_main_high_1.csv")))
dt <- dt[order(ID),]


# Size --------------------------------------------------------------------

dt.sub <- dt[dtau == 0 & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "size")
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

dt.sub2 <- matrix(NA, 27 + 3*3, ncol(dt.sub)+3)
dt.sub2[-c(seq(2,nrow(dt.sub2),12),
           seq(7,nrow(dt.sub2),12),
           seq(12,nrow(dt.sub2),12)),
        -c(4,8,12)] <- as.matrix(dt.sub[d <= 25,])

dt.sub3 <- matrix(NA, 5 + 2, ncol(dt.sub)+3)
dt.sub3[-c(2,7),
        -c(4,8,12)] <- as.matrix(dt.sub[d == 50,])

dt.sub <- rbind(dt.sub2,dt.sub3)

nn2 <- rep(" ",ncol(dt.sub))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub) <- nn2
print(xtable(dt.sub, digits = 3), include.rownames=FALSE)


# power -------------------------------------------------------------------
## single

dt.sub <- dt[dtau == .1 & test %in% main_tests & sigma != "weird" & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:3)]) := round(.SD,3), .SDcols=nn[-c(1:3)]]

dt.sub2 <- matrix(NA, 27 + 3*3, ncol(dt.sub)+3)
dt.sub2[-c(seq(2,nrow(dt.sub2),12),
           seq(7,nrow(dt.sub2),12),
           seq(12,nrow(dt.sub2),12)),
        -c(4,8,12)] <- as.matrix(dt.sub[d <= 25,])

dt.sub3 <- matrix(NA, 5 + 2, ncol(dt.sub)+3)
dt.sub3[-c(2,7),
        -c(4,8,12)] <- as.matrix(dt.sub[d == 50,])

dt.sub <- rbind(dt.sub2,dt.sub3)

nn2 <- rep(" ",ncol(dt.sub))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub) <- nn2
print(xtable(dt.sub, digits = 3), include.rownames=FALSE)


## column

dt.sub <- dt[dtau == .1 & test %in% main_tests & sigma != "weird" & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")

nn <- names(dt.sub)
dt.sub[,(nn[-c(1:3)]) := round(.SD,3), .SDcols=nn[-c(1:3)]]

dt.sub2 <- matrix(NA, 27 + 3*3, ncol(dt.sub)+3)
dt.sub2[-c(seq(2,nrow(dt.sub2),12),
           seq(7,nrow(dt.sub2),12),
           seq(12,nrow(dt.sub2),12)),
        -c(4,8,12)] <- as.matrix(dt.sub[d <= 25,])

dt.sub3 <- matrix(NA, 5 + 2, ncol(dt.sub)+3)
dt.sub3[-c(2,7),
        -c(4,8,12)] <- as.matrix(dt.sub[d == 50,])

dt.sub <- rbind(dt.sub2,dt.sub3)

nn2 <- rep(" ",ncol(dt.sub))
nn2[-c(4,8,12)] <- nn
colnames(dt.sub) <- nn2
print(xtable(dt.sub, digits = 3), include.rownames=FALSE)








main_tests <- c("euc","maha","maha_p","maha_d")

# Size --------------------------------------------------------------------

dt.sub <- dt[dtau == 0 & test %in% main_tests & sigma == "plugin" & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "size")
dt.sub


# power -------------------------------------------------------------------
## single

dt.sub <- dt[dtau == .1 & test %in% main_tests & sigma == "plugin" & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")
dt.sub


## column

dt.sub <- dt[dtau == .1 & test %in% main_tests & sigma == "plugin" & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")
dt.sub



main_tests <- c("sup","sups","sup_p","sup_d")

# Size --------------------------------------------------------------------

dt.sub <- dt[dtau == 0 & test %in% main_tests & sigma == "plugin" & distribution == "normal", .(size = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "size")
dt.sub


# power -------------------------------------------------------------------
## single

dt.sub <- dt[dtau == .1 & test %in% main_tests & sigma == "plugin" & distribution == "normal" & dtau_type == "single", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")
dt.sub


## column

dt.sub <- dt[dtau == .1 & test %in% main_tests & sigma == "plugin" & distribution == "normal" & dtau_type == "column", .(power = mean(pval < .05)), .(distribution, tau, sigma, test, n, d)]
dt.sub <- dcast(dt.sub, d + sigma + test ~ n + tau, value.var = "power")
dt.sub

