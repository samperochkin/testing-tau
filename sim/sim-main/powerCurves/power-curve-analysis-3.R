

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(latex2exp)
library(facetscales)



# Load results ------------------------------------------------------------
full.grid <- fread("sim/sim-main/powerCurves/results-3.csv")



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
full.grid$distribution <- factor(full.grid$distribution, levels = c("normal", "t4", "gumbel", "clayton"))
full.grid$dtau_type <- factor(full.grid$dtau_type,levels = c("single","column"))
full.grid$S <- factor(full.grid$S,levels = c("Sh","I"))
full.grid$norm <- factor(full.grid$norm)


# Comparison of distributions ---------------------------------------------

ta <- .3
d0 <- 15
dtau_t <- "column"

ggplot(full.grid[tau == ta & dtau_type == dtau_t & d == d0],
       aes(x=epsilon, y=power, col=distribution)) +
  ggtitle(paste(ta, dtau_t, d0)) +
  xlab(bquote(epsilon)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black')) +
  geom_line() +
  ###### ADJUST THESE LINES ####
  coord_cartesian(xlim = c(0,4.5)) +
  scale_x_continuous(breaks = c(0, 2.5, 5, 7.5, 10), labels = c("0", "2.5", "5", "7.5", "10")) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  ##############################
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  facet_grid(S~norm)






########
########
########
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
coord_cartesian(xlim = c(0,4.5)) +
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










####
#### Plot for paper
####

# Load results ------------------------------------------------------------
full.grid <- fread("sim/sim-main/powerCurves/results-3.csv")
al <- .05
full.grid <- full.grid[round(alpha,3)==al & d %in% ds & tau %in% taus]
full.grid$distribution <- factor(full.grid$distribution, levels = c("normal", "t4", "gumbel", "clayton"))
full.grid$dtau_type <- factor(full.grid$dtau_type,levels = c("single","column"))
full.grid$S <- factor(full.grid$S,levels = c("Sh","I"))
full.grid$norm <- factor(full.grid$norm)

small.grid <- full.grid[d %in% c(15,25) & distribution %in% c("normal", "t4") & tau == ta]
small.grid[, facet_labels := paste(dtau_t_lookup[dtau_type], "~(",
                                  dist_lookup[distribution], ")")]
unique(small.grid$facet_labels)
small.grid$facet_labels <- factor(small.grid$facet_labels,
                                  levels = unique(small.grid$facet_labels)[c(1,3,2,4)])
small.grid$d <- factor(small.grid$d, levels=c(15,25), labels=c(TeX("d = 15"), paste(TeX("d = 25"))))

ta <- .3
d_lookup <- c("15" = "d = 15", "25" = "d = 25")
dist_lookup <- c("normal" = "Normal", "t4" = "t[4]")
dtau_t_lookup <- c("single" = "single~dep.", "column" = "column~dep.")


ggplot(small.grid, aes(x=epsilon, y=power, shape=S, linetype=norm)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black'),
        # legend.text = element_text(size = 10),
        # legend.position = c(.935,.4),
        legend.position = "none",
        # legend.background = element_rect(fill = "gray85"),
        # legend.margin = margin(3,3,3,3, unit = "pt"),
        # legend.spacing = unit(50, "pt"),
        # legend.box.just =  "top",
        strip.text.x = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  # legend.position = "bottom",
  # legend.direction = "vertical",
  # legend.box = "horizontal") +
  geom_line() +
  geom_point(data = small.grid[epsilon %in% c(1,2,3,4,5,6,7,8,9,10)],
             aes(x=epsilon, y=power, shape=S), size=1.5) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(0,7.5)) +
  scale_x_continuous(breaks = c(0, 2.5, 5, 7.5, 10), labels = c("0", "2.5", "5", "7.5", "10")) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  ##############################
scale_shape_manual(breaks = c("I", "Sh"), values = c(16,17),
                   labels = c(expression(paste("(1/n)", I[p])), expression(hat(Sigma)[np]))) +
  scale_linetype_manual(name = "norm (statistic)", breaks = c("Euclidean", "Supremum"), values=c(1:2),
                        labels = c(expression(paste("Euclidean (", E["np"], ")")), expression(paste("Supremum (", M["np"], ")")))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  facet_grid(d~facet_labels, labeller = label_parsed)
