
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFunHigh.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFunHigh(n = c(125),
       d = c(35),
       tau = c(.3),
       dtau = c(0,.1),
       distribution = c("normal"),
       num_sim = 250,
       filename = "dt_main_high_test"
)
difftime(Sys.time(),tiime)


dt <- fread("dt_main_high_test.csv")
dt


library(ggplot2)

dt[,decision := pvalue < .05]
dt2 <- dt[,.("level" = mean(decision)), by=c("S","Sh","norm","dtau","dtau_type")]

ggplot(dt2, aes(x=S, y=level, fill=Sh)) +
        geom_bar(stat="identity",position=position_dodge2()) +
        geom_abline(intercept=.05, slope=0, lty=2, col="blue") +
        facet_grid(dtau+dtau_type~norm) + ylim(0,1)

