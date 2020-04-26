
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
setwd("sim-notPD")
source("simFunLow3.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFunLow3(n = c(250),
          d = c(10),
          tau = c(.6),
          dtau = c(0),
          distribution = c("normal"),
          num_sim = 100,
          filename = "dt_notPD_test",
          # clus = c(rep("dms11",8),rep(c("dms1","dms2","dms3","dms4","dms5","dms6","dms7"),4))
)
difftime(Sys.time(),tiime)


dt <- fread("dt_notPD_test.csv")
ggplot(dt,
       aes(x = pvalue)) + 
  geom_histogram(breaks = seq(0,1,.1), position = "stack",
                 aes(y=..count../sum(..count..))) +
  facet_grid(norm~Sh, scales="free")
dt[,decision := pvalue < .05]
dt2 <- dt[,.("level" = mean(decision,na.rm=T)), by=c("S","Sh","norm")]

ggplot(dt2, aes(x=S, y=level, fill=Sh)) +
  geom_bar(stat="identity",position=position_dodge2()) +
  geom_abline(intercept=.05, slope=0, lty=2, col="blue") +
  facet_grid(~norm) + ylim(0,1)


