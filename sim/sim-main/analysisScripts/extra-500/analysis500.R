# packages ----------------------------------------------------------------
library(data.table)
library(ggplot2)


# setup -------------------------------------------------------------------

# load appropriate file
fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
fii
dt <- fread(fii[grepl("500_1", fii)])
dt <- dt[, .(rejection_rate = mean(pvalue < .05)*100), .(n,d,tau,dtau,dtau_type,S,Sh,norm,distribution)]


# set some variables to factors for ggplot
dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt <- dt[S %in% c("Sh","I")] # restrict choice of S
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))
dt$norm <- factor(dt$norm)
dt$distribution <- factor(dt$distribution)




library(latex2exp)
ggplot(dt, aes(x=d, y=rejection_rate, shape=distribution, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_point() +
  geom_line() +
  xlab("dimension (d)") +
  ylab("rejection rate") +
  # scale_x_continuous(breaks = c(5, 15, 25, 50, 100)) +
  # scale_linetype_manual(name = "norm (statistic)", breaks = c("Euclidean", "Supremum"), values=c(1:2),
  #                       labels = c(expression(paste("Euclidean (", E["np"], ")")), expression(paste("Supremum (", M["np"], ")")))) +
  # scale_shape_manual(name = "copula", breaks = c("normal", "t4", "gumbel", "clayton"), values = c(16,15,17,3), labels = c("Normal", expression(paste("student's ", t[4])), "Gumbel", "Clayton")) +
  facet_grid(S~dtau_type, labeller = label_bquote(cols = paste(.(paste0(dtau_type)), " departure")))


ggplot(dt[n == 100 & dtau == .1 & tau == .3 & Sh == "SbJ" & distribution == "normal"],
       aes(x=d, y=rejection_rate, shape=S, linetype=norm)) +
  ggtitle(TeX("Power of the tests for exchangeability (H_0^*, Normal copula)")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black'),
        legend.text = element_text(size = 11)) +
  geom_point() +
  geom_line() +
  xlab("dimension (d)") +
  ylab("rejection rate") +
  scale_x_continuous(breaks = c(5, 15, 25, 50, 100)) +
  scale_linetype_manual(name = "norm (statistic)", breaks = c("Euclidean", "Supremum"), values=c(1,2),
                        labels = c(expression(paste("Euclidean (", E["np"], ")")), expression(paste("Supremum (", M["np"], ")")))) +
  scale_shape_manual(breaks = c("I", "Sh"), values = c(16,15),
                     labels = c(expression(paste("(1/n)", I[p])), expression(hat(Sigma)[np]))) +
  geom_hline(yintercept=.05, lty=3, col="gray25") +
  facet_grid(~dtau_type, labeller = label_bquote(cols = paste(.(paste0(dtau_type)), " departure")))


ggplot(dt[n == 100 & dtau == .1 & tau == .3 & Sh == "SbJ" & distribution == "normal"],
       aes(x=d, y=rejection_rate, shape=S, linetype=norm)) +
  ggtitle(TeX("Power of the tests for exchangeability (H_0^*, Normal copula)")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black'),
        legend.text = element_text(size = 11)) +
  geom_point() +
  geom_line() +
  xlab("dimension (d)") +
  ylab("rejection rate") +
  scale_x_continuous(breaks = c(5, 15, 25, 50, 100)) +
  scale_linetype_manual(name = "norm (statistic)", breaks = c("Euclidean", "Supremum"), values=c(1,2),
                        labels = c(expression(paste("Euclidean (", E["np"], ")")), expression(paste("Supremum (", M["np"], ")")))) +
  scale_shape_manual(breaks = c("I", "Sh"), values = c(16,15),
                     labels = c(expression(paste("(1/n)", I[p])), expression(hat(Sigma)[np]))) +
  geom_hline(yintercept=.05, lty=3, col="gray25") +
  facet_grid(~dtau_type, labeller = label_bquote(cols = paste(.(paste0(dtau_type)), " departure")))
