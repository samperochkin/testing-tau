# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)
library(latex2exp)
library(gtable)



# Load results ------------------------------------------------------------
filenames <- list.files("sim/sim-main/powerCurves3/results", recursive = T, full.names = T)
full.grid <- rbindlist(lapply(filenames, fread))

### TEST
filenames <- list.files("sim/sim-main/powerCurves/results", recursive = T, full.names = T)
full.grid <- rbindlist(lapply(filenames, fread))
full.grid$set_id <- 1

filenames <- list.files("sim/sim-main/powerCurves2/results", recursive = T, full.names = T)
full.grid2 <- rbindlist(lapply(filenames, fread))
full.grid2$set_id <- 2
full.grid <- rbind(full.grid, full.grid2)
rm(full.grid2)

filenames <- list.files("sim/sim-main/powerCurves3/results", recursive = T, full.names = T)
full.grid2 <- rbindlist(lapply(filenames, fread))
full.grid2$set_id <- 3
full.grid <- rbind(full.grid, full.grid2)
rm(full.grid2)

full.grid <- full.grid[distribution %in% c("normal", "t4")]
# full.grid <- full.grid[set_id %in% 2:3]

cnames <- colnames(full.grid)
cnames <- cnames[-(length(cnames) - (1:0))]
full.grid2 <- full.grid[, .(power = mean(power)), by = cnames]
full.grid2$set_id <- 4

full.grid <- rbind(full.grid,full.grid2)
# full.grid <- full.grid2
# rm(full.grid2)
###

source("sim/sim-main/analysisScripts/tables/functions/constructDTLocal.R")
fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)
fii <- fii[grepl("local", fii)]
emp.grid <- contructDTLocal(fii)
emp.grid$epsilon <- emp.grid$delta
emp.grid$power <- emp.grid$rejection_rate/100

# Setup -------------------------------------------------------------------
al <- .05
taus <- .3
ds <- c(5,15,25)
ns <- unique(emp.grid$n)

full.grid <- full.grid[round(alpha,3)==al & d %in% ds & tau %in% taus]
full.grid$distribution <- factor(full.grid$distribution, levels = c("normal", "t4", "gumbel", "clayton"))
full.grid$dtau_type <- factor(full.grid$dtau_type,levels = c("single","column"))
full.grid$S <- factor(full.grid$S,levels = c("Sh","I"))
full.grid$norm <- factor(full.grid$norm)

small.grid <- full.grid[d %in% ds & distribution %in% c("normal", "t4") & tau == taus]
S_lab <- c("Sh" = "S == hat(Sigma)[np]", "I" = "S == (1/n)~I[p]")
small.grid[, S_lab := as.factor(S_lab[as.character(S)])]
emp.grid[, S_lab := as.factor(S_lab[as.character(S)])]

norm_lab <- c("Euclidean" = "Euclidean~norm~(E[np])",
              "Supremum" = "Supremum~norm~(M[np])")
small.grid[, norm_lab := as.factor(norm_lab[as.character(norm)])]
emp.grid[, norm_lab := as.factor(norm_lab[as.character(norm)])]

d_lab <- c("5" = "d == 5", "15" = "d == 15", "25" = "d == 25")
small.grid[, d_lab := as.factor(d_lab[as.character(d)])]
small.grid$d_lab


# actual plots for paper (8) ----------------------------------------------
# n0 <- c(50,150,250,500,1000)
n0 <- ns
Sh0 <- c("ShJ", "SbJ")
d0 <- c(15)

# plot 1
dtau_type0 <- "single"
dis0 <- "normal"

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
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = n0, label = n0) +
  ###### ADJUST THESE LINES ####
  coord_cartesian(xlim = c(1,3)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
  geom_line(aes(col=as.factor(set_id))) +
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
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = n0, label = n0) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(1,3)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
  geom_line(aes(col=as.factor(set_id))) +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)


# plot 5
dtau_type0 <- "column"
dis0 <- "normal"

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
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = n0, label = n0) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(1,3)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
  geom_line(aes(col=as.factor(set_id))) +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)


# plot 6
dtau_type0 <- "column"
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
  scale_alpha_continuous(name = "sample size (n)", range = c(.25,1), breaks = n0, label = n0) +
  ###### ADJUST THESE LINES ####
coord_cartesian(xlim = c(1,3)) +
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10)) +
  scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "0.5", "1")) +
  #############################
  geom_line(aes(col=as.factor(set_id))) +
  geom_point(data=emp.grid[distribution == dis0 & dtau_type == dtau_type0 & d == d0 &
                             Sh %in% Sh0 & n %in% n0][tau+delta/sqrt(n) < 1],
             aes(alpha=n, shape=Sh), size = 2.5) +
  facet_grid(S_lab~norm_lab, labeller = label_parsed)


