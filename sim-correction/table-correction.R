setwd("sim-correction")
library(data.table)
library(xtable)
# dt <- fread("dt_correction_1.csv")
dt <- fread("dt_correction_2.csv")
dt <- dt[order(ID),]

dt[,isNA := is.na(pval)]
dt.sub <- dt[dtau == 0 & test == "sup" & force_constraints == F, .(meanNA = round(mean(isNA),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "meanNA")
dt.sub


# Size --------------------------------------------------------------------

# sup
dt.sub <- dt[dtau == 0  & test == "sup"  & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction, force_constraints)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction + force_constraints, value.var = "size")
dt.sub

dt.sub <- dt[dtau == 0  & test == "sup"  & distribution == "normal", .(size = round(mean(pval < .05, na.rm = T),3)), .(tau, n, d, correction, force_constraints)]
dt.sub <- dcast(dt.sub, tau + d + n  ~ correction + force_constraints, value.var = "size")
dt.sub


# euc
dt.sub <- dt[dtau == 0  & test == "euc"  & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction, force_constraints)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction + force_constraints, value.var = "size")
dt.sub


# supS
dt.sub <- dt[dtau == 0  & test == "sups"  & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction, force_constraints)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction + force_constraints, value.var = "size")
dt.sub


# maha
dt.sub <- dt[dtau == 0  & test == "maha"  & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction, force_constraints, sigma)]
dt.sub <- dcast(dt.sub, sigma + tau + d + n ~ correction + force_constraints, value.var = "size")
dt.sub






# Power --------------------------------------------------------------------
dt <- fread("dt_correction_3.csv")
dt <- dt[order(ID),]

dt[,isNA := is.na(pval)]
dt.sub <- dt[dtau == .1 & dtau_type == "single" & test == "sup" & force_constraints == F, .(meanNA = round(mean(isNA),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "meanNA")
dt.sub


#### single
# sup
dt.sub <- dt[dtau == .1 & dtau_type == "single" & test == "sup" & force_constraints == F & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "size")
dt.sub


# euc
dt.sub <- dt[dtau == .1 & dtau_type == "single" & test == "euc" & force_constraints == F & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "size")
dt.sub


# supS
dt.sub <- dt[dtau == .1 & dtau_type == "single" & test == "sups" & force_constraints == F & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "size")
dt.sub


# maha
dt.sub <- dt[dtau == .1 & dtau_type == "single" & test == "maha" & force_constraints == F  & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "size")
dt.sub



#### column
# sup
dt.sub <- dt[dtau == .1 & dtau_type == "column" & test == "sup" & force_constraints == F & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "size")
dt.sub


# euc
dt.sub <- dt[dtau == .1 & dtau_type == "column" & test == "euc" & force_constraints == F & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "size")
dt.sub


# supS
dt.sub <- dt[dtau == .1 & dtau_type == "column" & test == "sups" & force_constraints == F & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "size")
dt.sub


# maha
dt.sub <- dt[dtau == .1 & dtau_type == "column" & test == "maha" & force_constraints == F  & distribution == "normal", .(size = round(mean(pval < .05),3)), .(tau, n, d, correction)]
dt.sub <- dcast(dt.sub, tau + d + n ~ correction, value.var = "size")
dt.sub




##############
dt <- fread("dt_correction_2.csv")
dt <- dt[order(ID),]
dt.sub <- dt[dtau == 0  & distribution == "normal" & correction == T, .(size = round(mean(pval < .05),3)), .(tau, n, d, test, force_constraints)]
dt.sub <- dcast(dt.sub, force_constraints + tau + d + n ~ test, value.var = "size")
dt.sub

dt <- fread("dt_correction_3.csv")
dt <- dt[order(ID),]
dt.sub <- dt[dtau == .1  & distribution == "normal" & correction == T & force_constraints == T, .(size = round(mean(pval < .05),3)), .(tau, n, d, test, dtau_type)]
dt.sub <- dcast(dt.sub, dtau_type + tau + d + n ~ test, value.var = "size")
dt.sub
