

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(latex2exp)
library(facetscales)



# Load results ------------------------------------------------------------
filenames <- list.files("sim/sim-main/powerCurves3/results", recursive = T, full.names = T)
full.grid <- rbindlist(lapply(filenames, fread))

source("sim/sim-main/analysisScripts/tables/functions/constructDTLocal.R")
fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
fii <- fii[grepl("local", fii)]
dt <- contructDTLocal(fii)



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
ta <- .3
d_lookup <- c("5" = "d = 5", "15" = "d = 15", "25" = "d = 25")
dist_lookup <- c("normal" = "Normal", "t4" = "t[4]")
dtau_t_lookup <- c("single" = "single~dep.", "column" = "column~dep.")

ds <- c(5, 25)

small.grid <- full.grid[d %in% ds & distribution %in% c("normal", "t4") & tau == ta]
small.grid[, facet_labels := paste(dtau_t_lookup[as.character(dtau_type)], "~(",
                                  dist_lookup[as.character(distribution)], ")")]
unique(small.grid$facet_labels)
small.grid$facet_labels <- factor(small.grid$facet_labels,
                                  levels = unique(small.grid$facet_labels)[c(1,3,2,4)])
small.grid$d <- factor(small.grid$d, levels=ds, labels=c(TeX("d = 5"), paste(TeX("d = 25"))))


ggplot(small.grid, aes(x=epsilon, y=power, shape=S, linetype=norm)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black'),
        legend.position = "none",
        strip.text.x = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
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



####
#### Plots for appendix
####
source("sim/sim-main/analysisScripts/tables/functions/constructDTLocal.R")
fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
fii <- fii[grepl("local", fii)]
emp.grid <- contructDTLocal(fii)
emp.grid$epsilon <- emp.grid$delta
emp.grid$power <- emp.grid$rejection_rate/100

emp.grid0 <- emp.grid[dtau_type == "none"]
emp.grid0$dtau_type <- "single"
emp.grid <- rbind(emp.grid, emp.grid0)
emp.grid0$dtau_type <- "column"
emp.grid <- rbind(emp.grid, emp.grid0)
emp.grid <- emp.grid[dtau_type %in% c("single", "column")]
rm(emp.grid0)

emp.grid <- emp.grid[d %in% ds & distribution %in% c("normal", "t4") & tau == ta]
emp.grid[, facet_labels := paste(dtau_t_lookup[as.character(dtau_type)], "~(",
                                 dist_lookup[as.character(distribution)], ")")]

cbind(emp.grid[1:10],emp.grid[1:10, paste(dtau_t_lookup[dtau_type], "~(",
                                 dist_lookup[distribution], ")")])

unique(emp.grid$facet_labels)
emp.grid$facet_labels <- factor(emp.grid$facet_labels,
                                  levels = unique(emp.grid$facet_labels)[4:1])
small.grid$d <- factor(small.grid$d, levels=ds, labels=c(TeX("d = 5"), paste(TeX("d = 25"))))


ggplot(small.grid, aes(x=epsilon, y=power, shape=S, linetype=norm)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray95"),
        strip.text = element_text(colour = 'black'),
        legend.position = "none",
        strip.text.x = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
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
