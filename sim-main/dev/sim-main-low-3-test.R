
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------

#**************#
## ** BOOT ** ##
#**************#
source("simFuns/simFunLow.R")
#**************#
## ** BOOT ** ##
#**************#

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLow(
       n = c(100),
       d = c(5),
       tau = c(.3),
       dtau = c(0),
       distribution = c("normal"),
       num_sim = 50,
       filename = "dt_main_low_3_test"
)
difftime(Sys.time(),tiime)


dt <- fread("dt_main_low_3_test.csv")
ggplot(dt[n == 100 & d == 5 & tau == 0.3 & isShPd == T], aes(col = norm)) + 
        stat_qq(aes(sample=pvalue), distribution = qunif) +
        #scale_y_continuous(breaks = seq(.25, .75, by = .5)) +
        ylim(0,1) +
        facet_grid(S~Sh)
ggplot(dt[n == 100 & d == 5 & tau == 0.3], aes(col = norm)) + 
        stat_qq(aes(sample=pvalue), distribution = qunif) +
        #scale_y_continuous(breaks = seq(.25, .75, by = .5)) +
        ylim(0,1) +
        facet_grid(S~Sh)

dt[, .(mna = mean(is.na(pvalue))), by = .(S,Sh,isShPd)]


dt[,decision := pvalue < .05]
dt2 <- dt[,.("level" = mean(decision,na.rm=T)), by=c("n","d","tau",
                                                     "S","Sh","norm","dtau","dtau_type","pvalue_method")]
ggplot(dt2[n == 150 & d == 5 & tau == 0.3], aes(x=S, y=level, fill=Sh)) +
        geom_bar(stat="identity",position=position_dodge2()) +
        geom_abline(intercept=.05, slope=0, lty=2, col="blue") +
        facet_grid(dtau+dtau_type~norm) + ylim(0,1)

