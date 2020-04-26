
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
# setwd("sim-main")
# source("simFuns/simFunHigh2.R")
source("simFunHigh2.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFunHigh(n = c(50),
       d = c(25),
       tau = c(0),
       dtau = c(0),
       distribution = c("normal"),
       num_sim = 5000,
       filename = "dt_main_high2_test",
       clus = rep(c("dms11","dms12"),each=8)
)
difftime(Sys.time(),tiime)


dt <- fread("dt_main_high2_test.csv")

table(dt[dtau == 0 & S == "I" & norm == "Supremum"]$s_info)

library(ggplot2)

dt[,decision := pvalue < .05]
# dt2 <- dt[,.("level" = mean(decision)), by=c("S","Sh","norm","dtau","dtau_type")]
#
# ggplot(dt2, aes(x=S, y=level, fill=Sh)) +
#         geom_bar(stat="identity",position=position_dodge2()) +
#         geom_abline(intercept=.05, slope=0, lty=2, col="blue") +
#         facet_grid(dtau+dtau_type~norm) + ylim(0,1)


dt2 <- dt[dtau == 0 & S == "I",.("level" = mean(decision), "N" = .N), by=c("S","Sh","norm","s_info")]
ggplot(dt2, aes(x=norm, y=level, fill=Sh)) +
        geom_bar(stat="identity",position=position_dodge2()) +
        geom_abline(intercept=.05, slope=0, lty=2, col="blue") +
        facet_grid(~s_info) + ylim(0,1)

