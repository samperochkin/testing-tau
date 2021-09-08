

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)



# Load results ------------------------------------------------------------

filenames <- list.files("sim/sim-main/powerCurves/results", recursive = T, full.names = T)
full.grid <- rbindlist(lapply(filenames, fread))



# Setup -------------------------------------------------------------------
al <- .05
# ds <- sort(unique(full.grid$d))
ds <- c(5,15,25)
d_labels <- sapply(ds, function(d){
  paste0("d = ", d)
})
names(d_labels) <- ds

taus <- sort(unique(full.grid$tau))
tau_labels <- sapply(taus, function(tau0){
  eval(expression(tau0))
})
names(tau_labels) <- taus


full.grid <- full.grid[round(alpha,3)==al & d %in% ds & tau %in% taus]

# Comparison of distributions ---------------------------------------------

#######################
ta <- .5
#######################

#######################
dtau_t <- "single"

ggplot(full.grid[tau == ta & dtau_type == dtau_t],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  ggtitle(parse(text = paste0("Power~curves~(tau*'='*",ta, "*','","~", dtau_t, "~departure)"))) +
  xlab(bquote(epsilon)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
###### ADJUST THESE LINES ####
  coord_cartesian(xlim = c(0,7)) +
  scale_x_continuous(breaks = c(0, 2.5, 5, 7.5, 10), labels = c("0", "2.5", "5", "7.5", "10")) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
##############################
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(breaks = c("I", "Sh"), values = 1:2, labels = c(bquote(I), bquote(Sigma))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  facet_grid(d~distribution, labeller = label_bquote(rows = `d` == .(d),
                                            cols = .(distribution)))

#######################
dtau_t <- "column"

ggplot(full.grid[tau == ta & dtau_type == dtau_t],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  ggtitle(parse(text = paste0("Power~curves~(tau*'='*",ta, "*','","~", dtau_t, "~departure)"))) +
  xlab(bquote(epsilon)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  ###### ADJUST THESE LINES ####
  coord_cartesian(xlim = c(0,4)) +
  scale_x_continuous(breaks = c(0, 1, 2, 3), labels = c("0", "1", "2", "3")) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  ##############################
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(breaks = c("I", "Sh"), values = 1:2, labels = c(bquote(I), bquote(Sigma))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  facet_grid(d~distribution, labeller = label_bquote(rows = `d` == .(d),
                                                     cols = .(distribution)))



# (distribution-departure) specific results -------------------------------

#######################
distri <- "clayton"
dtau_t <- "column"
#######################

dist_text <- c("Normal", expression("t"[4]), "Gumbel", "Clayton")[match(distri, c("normal", "t4", "gumbel", "clayton"))]


ggplot(full.grid[distribution == distri & dtau_type == dtau_t],
       aes(x=epsilon, y=power, col=norm, linetype=S)) +
  ggtitle(parse(text = paste0("Power~curves~(", dist_text, "~copula*','","~", dtau_t, "~departure)"))) +
  xlab(bquote(epsilon)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  scale_x_continuous(breaks = c(0, 2.5, 5, 7.5, 10), labels = c("0", "2.5", "5", "7.5", "10")) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(breaks = c("I", "Sh"), values = 1:2, labels = c(bquote(I), bquote(Sigma))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  facet_grid(d~tau, labeller = label_bquote(rows = `d` == .(d),
                                            cols = `tau` == .(tau)))

ggplot(full.grid[round(alpha,3)==al & dtau_type %in% dtau_t & distribution %in% distri,
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(tau~norm+S)




#####################################

ggplot(full.grid[round(alpha,3)==al & dtau_type %in% dtau_t & distribution %in% distri,
                 .(power = mean(power)),
                 .(norm, S, d, tau)],
       aes(x=d, y=power, col=as.factor(tau))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  facet_grid(norm~S)

