# packages ----------------------------------------------------------------
library(data.table)
library(ggplot2)


# setup -------------------------------------------------------------------

# load appropriate file
fii <- list.files("sim/sim-main/results",full.names = T)
fii
dt <- fread(fii[grepl("exch_full", fii)])

# set some variables to factors for ggplot
dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt <- dt[S %in% c("Sh","I")] # restrict choice of S
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))
dt$norm <- factor(dt$norm)


# plots H0 ----------------------------------------------------------------


# name statistic ** 
# have H0 in facet title

ggplot(dt[n == 100 & dtau == .1 & tau == .3 & Sh == "ShJ" & S == "I"],
       aes(x=d, y=rejection_rate, shape=distribution, linetype=norm)) +
  ggtitle(expression(paste("Power of the tests for equicorrelation (", H[0], ") with S=(1/n)", I[p]))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_point() +
  geom_line() +
  xlab("dimension (d)") +
  ylab("rejection rate") +
  # ylab(expression(paste("rejection rate (", hat(alpha), ")"))) +
  scale_x_continuous(breaks = c(5, 15, 25, 50, 100)) +
  # scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  # scale_color_manual(values = c("red", "blue")) +
  # scale_linetype_manual(breaks = c("I", "Sh"), values = 1:2, labels = c(bquote(I), bquote(Sigma))) +
  scale_linetype_manual(name = "norm (statistic)", breaks = c("Euclidean", "Supremum"), values=c(1:2),
                        labels = c(expression(paste("Euclidean (", E["np"], ")")), expression(paste("Supremum (", M["np"], ")")))) +
  scale_shape_manual(name = "copula", breaks = c("normal", "t4", "gumbel", "clayton"), values = c(16,15,17,3), labels = c("Normal", expression(paste("student's ", t[4])), "Gumbel", "Clayton")) +
  # geom_vline(xintercept=0) +
  # geom_hline(yintercept=0) +
  geom_hline(yintercept=.05, lty=3, col="gray25") +
  # facet_grid(~dtau_type, labeller = label_bquote(cols = paste(.(paste0(dtau_type)), " departure (", H[0], ")")))
  facet_grid(~dtau_type, labeller = label_bquote(cols = paste(.(paste0(dtau_type)), " departure")))




# Plots H0 star -----------------------------------------------------------

ggplot(dt[n == 100 & dtau == .1 & tau == .3 & Sh == "SbJ"],
       aes(x=d, y=rejection_rate, shape=distribution, linetype=norm)) +
  ggtitle(TeX("Power of the tests for exchangeability (H_0^*)")) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_point() +
  geom_line() +
  xlab("dimension (d)") +
  ylab("rejection rate") +
  # ylab(expression(paste("rejection rate (", hat(alpha), ")"))) +
  scale_x_continuous(breaks = c(5, 15, 25, 50, 100)) +
  # scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  # scale_color_manual(values = c("red", "blue")) +
  # scale_linetype_manual(breaks = c("I", "Sh"), values = 1:2, labels = c(bquote(I), bquote(Sigma))) +
  scale_linetype_manual(name = "norm (statistic)", breaks = c("Euclidean", "Supremum"), values=c(1:2),
                        labels = c(expression(paste("Euclidean (", E["np"], ")")), expression(paste("Supremum (", M["np"], ")")))) +
  scale_shape_manual(name = "copula", breaks = c("normal", "t4", "gumbel", "clayton"), values = c(16,15,17,3), labels = c("Normal", expression(paste("student's ", t[4])), "Gumbel", "Clayton")) +
  # geom_vline(xintercept=0) +
  # geom_hline(yintercept=0) +
  geom_hline(yintercept=.05, lty=3, col="gray25") +
  # facet_grid(~dtau_type, labeller = label_bquote(cols = paste(.(paste0(dtau_type)), " departure (", H[0], ")")))
  facet_grid(S~dtau_type, labeller = label_bquote(cols = paste(.(paste0(dtau_type)), " departure")))
# arrange row strips if this is chosen


library(latex2exp)
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
