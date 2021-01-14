
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
setwd("sim-main")
source("simFuns/simFunHigh2.R")
# source("simFunHigh2.R")

# procedure ---------------------------------------------------------------

# set wd appropriately and
# launch with nohup R CMD BATCH sim-low-dim.R log.txt &
# should take much less than a day

tiime <- Sys.time()
simFunHigh2(n = c(100),
       d = c(50),
       tau = c(.3),
       dtau = c(0),
       distribution = c("normal"),
       num_sim = 100,
       filename = "dt_main_high2_test",
       # clus = rep(c("dms11","dms12"),each=8)
)
difftime(Sys.time(),tiime)




df <- fread("dt_main_high2_test.csv")

library(ggplot2)

df[,decision := pvalue < .05]


# dt2 <- dt[dtau == 0 & S == "I",.("level" = mean(decision), "N" = .N), by=c("S","Sh","norm","s_info")]
df2 <- df[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)), "NN" = .N),
          # by=c("n","d","tau","S","Sh","norm","dtau","dtau_type")]
          by=c("n","d","tau","S","Sh","norm","dtau")]
df2 <- df[S == "Sh",.("level" = mean(decision), "N" = .N), by=c("S","Sh","norm","dtau","d","n","tau")]
ggplot(df2, aes(x=norm, y=level, fill=Sh)) +
        geom_bar(stat="identity",position=position_dodge2()) +
        geom_abline(intercept=.05, slope=0, lty=2, col="blue") +
        ylim(0,1)

xdf <- dcast(df2[norm == "Euclidean" & S == "Sh" & dtau == 0],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdf


xdf <- dcast(df2[norm == "Supremum" & S == "Sh" & dtau == 0],
             formula = Sh + d ~ tau + n,
             value.var = "level")
xdf
