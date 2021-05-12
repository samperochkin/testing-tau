

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)



# Load results ------------------------------------------------------------

full.grid <- fread("sim/sim-main/powerCurves/full_grid_2.csv")


# Plots -------------------------------------------------------------------

# Plots in main text
ds <- sort(unique(full.grid$d))
d_labels <- sapply(ds, function(d){
  paste0("d = ", d)
})
names(d_labels) <- ds

taus <- sort(unique(full.grid$tau))
tau_labels <- sapply(taus, function(tau0){
  eval(expression(tau))
})
names(tau_labels) <- taus

# single - normal
al <- .05
ggplot(full.grid[round(alpha,3)==al & distribution == "normal" & dtau_type == "single"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  ggtitle("Power curves (Normal copula, single departure)") +
  xlab(bquote(epsilon)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(breaks = c("I", "Sh"), values = 1:2, labels = c(bquote(I), bquote(Sigma))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  ylim(c(0,1)) +
  facet_grid(d~tau, labeller = label_bquote(rows = `d` == .(d),
                                            cols = `tau` == .(tau)))

ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "normal"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  ggtitle("Power curves (Normal copula, column departure)") +
  xlab(bquote(epsilon)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(breaks = c("I", "Sh"), values = 1:2, labels = c(bquote(I), bquote(Sigma))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  ylim(c(0,1)) +
  xlim(c(0,5)) +
  facet_grid(d~tau, labeller = label_bquote(rows = `d` == .(d),
                                            cols = `tau` == .(tau)))








ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "normal",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(tau~norm+S)

ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "normal",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=tau, y=power)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(d~norm+S)
#



ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "joe",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(tau~norm+S)

ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "joe",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=tau, y=power)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(d~norm+S)









# single - normal
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "normal"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)


# column - normal
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "normal"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)


# single - supremum
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & norm == "Supremum"],
       aes(x=epsilon, y=power, col=distribution, linetype=S)) +
  ggtitle("Power curves (Supremum-based stats, single departure)") +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  xlim(c(0,5)) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# single - euclidean
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & norm == "Euclidean"],
       aes(x=epsilon, y=power, col=distribution, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# column - supremum
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & norm == "Supremum"],
       aes(x=epsilon, y=power, col=distribution, linetype=S)) +
  ggtitle("Power curves (Supremum-based stats, column departure)") +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  xlim(c(0,10)) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# column - euclidean
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & norm == "Euclidean"],
       aes(x=epsilon, y=power, col=distribution, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)



# single - cauchy
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "cauchy"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)

# column - cauchy
al <- .05
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "cauchy"],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  theme_light() +
  geom_line() +
  geom_hline(yintercept=al, lty=2) +
  ylim(c(0,1)) +
  facet_wrap(d~tau)



#####################################

ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "joe",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power, col=as.factor(tau))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "normal",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power, col=as.factor(tau))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "cauchy",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power, col=as.factor(tau))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)



ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "joe",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=tau, y=power, col=as.factor(d))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "normal",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=tau, y=power, col=as.factor(d))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
ggplot(full.grid[round(alpha,3)==al & dtau_type == "single" & distribution == "cauchy",
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=tau, y=power, col=as.factor(d))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)



#### COLUMN EPS LESS THAN 5

ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "joe" & epsilon <= 5,
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power, col=as.factor(tau))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "normal" & epsilon <= 5,
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power, col=as.factor(tau))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "cauchy" & epsilon <= 5,
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power, col=as.factor(tau))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)


ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "joe" & epsilon <= 5,
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=tau, y=power, col=as.factor(d))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "normal" & epsilon <= 5,
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=tau, y=power, col=as.factor(d))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
ggplot(full.grid[round(alpha,3)==al & dtau_type == "column" & distribution == "cauchy" & epsilon <= 5,
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=tau, y=power, col=as.factor(d))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)
