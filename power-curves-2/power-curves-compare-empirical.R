

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(latex2exp)
library(gtable)



# Load results ------------------------------------------------------------
full.grid <- fread("powerCurves/results-3.csv")
emp.grid <- fread("sim/sim-main/results/results-extra/dt_local_cc.csv")
emp.grid$epsilon <- emp.grid$delta
emp.grid$power <- emp.grid$rejection_rate/100


# Setup -------------------------------------------------------------------
al <- .05
# ds <- sort(unique(full.grid$d))
ds <- c(5,25)
d_labels <- sapply(ds, function(d){
  paste0("d = ", d)
})
names(d_labels) <- ds

taus <- sort(unique(full.grid$tau))
tau_labels <- sapply(taus, function(tau0){
  eval(expression(tau0))
})
names(tau_labels) <- taus


####
al <- .05
taus <- .3
ds <- c(5,25)
ns <- c(250, 500, 1000, 1500)

full.grid <- full.grid[round(alpha,3)==al & d %in% ds & tau %in% taus]
full.grid$distribution <- factor(full.grid$distribution, levels = c("normal", "t4", "gumbel", "clayton"))
full.grid$dtau_type <- factor(full.grid$dtau_type,levels = c("single","column"))
full.grid$S <- factor(full.grid$S,levels = c("Sh","I"))
full.grid$norm <- factor(full.grid$norm)

ta <- .3
d0 <- 25
# dtau_t <- "column"
d_lookup <- c("5" = "d = 5", "25" = "d = 25")
dtau_t_lookup <- c("single" = "single~dep.", "column" = "column~dep.")


small.grid <- full.grid[d %in% c(5,25) & distribution %in% c("normal", "t4") & tau == ta]
small.grid[, facet_labels := paste(dtau_t_lookup[dtau_type], "~(",
                                   dist_lookup[distribution], ")")]
unique(small.grid$facet_labels)
small.grid$facet_labels <- factor(small.grid$facet_labels,
                                  levels = unique(small.grid$facet_labels)[c(1,3,2,4)])
small.grid$d_lab <- factor(small.grid$d, levels=c(5,25), labels=c(TeX("d = 5"), paste(TeX("d = 25"))))



# test --------------------------------------------------------------------
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
  facet_grid(d_lab~facet_labels, labeller = label_parsed)

######
# contruct labels 
small.grid <- full.grid[d %in% c(5,25) & distribution %in% c("normal", "t4") & tau == ta]
# S_lab <- c("Sh" = "S = Sigma", "I" = "S = (1/n)I")
S_lab <- c("Sh" = "S == hat(Sigma)[np]", "I" = "S == (1/n)~I[p]")
small.grid[, S_lab := as.factor(S_lab[S])]
emp.grid[, S_lab := as.factor(S_lab[S])]

norm_lab <- c("Euclidean" = "Euclidean~norm~(E[np])",
              "Supremum" = "Supremum~norm~(M[np])")
# norm_lab <- c("Euclidean" = paste0("Euclidean~norm~(", expression(E[np]), ")"),
#               "Supremum" = paste0("Supremum~norm~(", expression(M[np]), ")"))
small.grid[, norm_lab := as.factor(norm_lab[norm])]
emp.grid[, norm_lab := as.factor(norm_lab[norm])]

d_lab <- c("5" = "d == 5", "25" = "d == 25")
small.grid[, d_lab := as.factor(d_lab[as.character(d)])]
small.grid$d_lab


# plot
dis0 <- "t4"
d0 <- 25
dtau_type0 <- "column"
n0 <- c(50,150,250)
Sh0 <- "ShJ"

Sh00 <- ifelse(Sh0 == "ShJ", "hat(Sigma)[np]^J", "bar(Sigma)[np]^J")
dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
# title <- paste0("distribution:~", dis00, "~~~~~~hat(Sigma)[np] == ", Sh00, "~~~~~~d == ", d)
title <- paste0(dtau_type0, "~departure:~~", dis00,"~copula~(d == ",d0, ")~~with~~hat(Sigma)[np] == ", Sh00)


small.emp.grid <- emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh == Sh0 & n %in% n0]

smallest.data <- small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0]
smallest.data$full_lab <- small.emp.grid$full_lab[1]
smallest.data$full_lab <- droplevels(smallest.data$full_lab)

g <- ggplot(smallest.data, aes(x=epsilon, y=power, shape=S, linetype=norm)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  ggtitle(parse(text=title)) +
  geom_line() +
  # geom_point(data = smallest.data[epsilon %in% c(1,2,3,4,5,6,7,8,9,10)],
  #            aes(x=epsilon, y=power, shape=S), size=1.5) +
  ###### ADJUST THESE LINES ####
  coord_cartesian(xlim = c(0,7.5)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  ##############################
  # scale_shape_manual(breaks = c("I", "Sh"), values = c(16,17),
  #                    labels = c(expression(paste("(1/n)", I[p])), expression(hat(Sigma)[np]))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  geom_point(data=small.emp.grid, aes(alpha=n), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)
# facet_nested(S_lab~full_lab+norm_lab, labeller = label_parsed)
g





# actual plots for paper (8) ----------------------------------------------
n0 <- c(50,150,250)
Sh0 <- c("ShJ", "SbJ")

# plot 1
dtau_type0 <- "single"
dis0 <- "normal"
d0 <- 5

dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
title <- paste0(dtau_type0, "~departure:~", dis00,"~copula~(d == ",d0, ")")
ggplot(small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0],
            aes(x=epsilon, y=power, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        # legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  guides(linetype="none") +
  ggtitle(parse(text=title)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  scale_shape_manual(name = expression(covariance~estimator~(hat(Sigma)[np])),
                     breaks = c("ShJ", "SbJ"), values = c(16,17),
                     labels = c(expression(hat(Sigma)[np]^J), expression(bar(Sigma)[np]^J))) +
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = c(50, 150, 250), label = c(50, 150, 250)) +
  ###### ADJUST THESE LINES ####
  coord_cartesian(xlim = c(0,5)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
  geom_line() +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)


# plot 2
dtau_type0 <- "single"
dis0 <- "t4"

dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
title <- paste0(dtau_type0, "~departure:~", dis00,"~copula~(d == ",d0, ")")
ggplot(small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0],
       aes(x=epsilon, y=power, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        # legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  guides(linetype="none") +
  ggtitle(parse(text=title)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  scale_shape_manual(name = expression(covariance~estimator~(hat(Sigma)[np])),
                     breaks = c("ShJ", "SbJ"), values = c(16,17),
                     labels = c(expression(hat(Sigma)[np]^J), expression(bar(Sigma)[np]^J))) +
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = c(50, 150, 250), label = c(50, 150, 250)) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(0,5)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
geom_line() +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)


# plot 3
dtau_type0 <- "single"
dis0 <- "normal"
d0 <- 25

dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
title <- paste0(dtau_type0, "~departure:~", dis00,"~copula~(d == ",d0, ")")
ggplot(small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0],
       aes(x=epsilon, y=power, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        # legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  guides(linetype="none") +
  ggtitle(parse(text=title)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  scale_shape_manual(name = expression(covariance~estimator~(hat(Sigma)[np])),
                     breaks = c("ShJ", "SbJ"), values = c(16,17),
                     labels = c(expression(hat(Sigma)[np]^J), expression(bar(Sigma)[np]^J))) +
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = c(50, 150, 250), label = c(50, 150, 250)) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(0,7)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
geom_line() +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)



# plot 4
dtau_type0 <- "single"
dis0 <- "t4"
d0 <- 25

dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
title <- paste0(dtau_type0, "~departure:~", dis00,"~copula~(d == ",d0, ")")
ggplot(small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0],
       aes(x=epsilon, y=power, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        # legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  guides(linetype="none") +
  ggtitle(parse(text=title)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  scale_shape_manual(name = expression(covariance~estimator~(hat(Sigma)[np])),
                     breaks = c("ShJ", "SbJ"), values = c(16,17),
                     labels = c(expression(hat(Sigma)[np]^J), expression(bar(Sigma)[np]^J))) +
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = c(50, 150, 250), label = c(50, 150, 250)) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(0,7)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
geom_line() +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)


# plot 5
dtau_type0 <- "column"
dis0 <- "normal"
d0 <- 5

dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
title <- paste0(dtau_type0, "~departure:~", dis00,"~copula~(d == ",d0, ")")
ggplot(small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0],
       aes(x=epsilon, y=power, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        # legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  guides(linetype="none") +
  ggtitle(parse(text=title)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  scale_shape_manual(name = expression(covariance~estimator~(hat(Sigma)[np])),
                     breaks = c("ShJ", "SbJ"), values = c(16,17),
                     labels = c(expression(hat(Sigma)[np]^J), expression(bar(Sigma)[np]^J))) +
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = c(50, 150, 250), label = c(50, 150, 250)) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(0,5)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
geom_line() +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)


# plot 6
dtau_type0 <- "column"
dis0 <- "t4"
d0 <- 5

dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
title <- paste0(dtau_type0, "~departure:~", dis00,"~copula~(d == ",d0, ")")
ggplot(small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0],
       aes(x=epsilon, y=power, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        # legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  guides(linetype="none") +
  ggtitle(parse(text=title)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  scale_shape_manual(name = expression(covariance~estimator~(hat(Sigma)[np])),
                     breaks = c("ShJ", "SbJ"), values = c(16,17),
                     labels = c(expression(hat(Sigma)[np]^J), expression(bar(Sigma)[np]^J))) +
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = c(50, 150, 250), label = c(50, 150, 250)) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(0,5)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
geom_line() +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)


# plot 7
dtau_type0 <- "column"
dis0 <- "normal"
d0 <- 25

dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
title <- paste0(dtau_type0, "~departure:~", dis00,"~copula~(d == ",d0, ")")
ggplot(small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0],
       aes(x=epsilon, y=power, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        # legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  guides(linetype="none") +
  ggtitle(parse(text=title)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  scale_shape_manual(name = expression(covariance~estimator~(hat(Sigma)[np])),
                     breaks = c("ShJ", "SbJ"), values = c(16,17),
                     labels = c(expression(hat(Sigma)[np]^J), expression(bar(Sigma)[np]^J))) +
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = c(50, 150, 250), label = c(50, 150, 250)) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(0,5)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
geom_line() +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)



# plot 8
dtau_type0 <- "column"
dis0 <- "t4"
d0 <- 25

dis00 <- ifelse(dis0 == "t4", "t[4]", "Normal")
title <- paste0(dtau_type0, "~departure:~", dis00,"~copula~(d == ",d0, ")")
ggplot(small.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0],
       aes(x=epsilon, y=power, linetype=norm)) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="gray90", colour="gray50"),
        # legend.position = "none",
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 11.5),
        strip.text.y = element_text(size = 11.5),
        axis.title=element_text(size=12)) +
  guides(linetype="none") +
  ggtitle(parse(text=title)) +
  xlab(bquote(paste("delta (", Delta, ")"))) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=al, lty=3, col="gray25") +
  scale_shape_manual(name = expression(covariance~estimator~(hat(Sigma)[np])),
                     breaks = c("ShJ", "SbJ"), values = c(16,17),
                     labels = c(expression(hat(Sigma)[np]^J), expression(bar(Sigma)[np]^J))) +
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = c(50, 150, 250), label = c(50, 150, 250)) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(0,5)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
geom_line() +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)

