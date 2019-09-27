library(data.table)
library(ggplot2)

res <- fread("frank1.csv")
res <- melt(res, measure.vars = c("maha","maha2","maha3",
                                  "euc","euc2","euc3",
                                  "sup","sup2","sup3",
                                  "supS","supS2","supS3"), variable.name = "loss_type", value.name = "loss")

res[grep("sup",loss_type), type2 := "unstand"]
res[grep("supS",loss_type), type2 := "stand"]

#mc_sup <- replicate(1000, {max(abs(rnorm(p)))}) # choose p

ggplot(res[grep("sup",loss_type)][dtau == 0 & tau == .75], aes(x=loss,fill=loss_type)) +
  geom_density(alpha = .75) +
  # geom_density(data = data.frame(loss = mc_sup, loss_type="mc"), aes(x=loss), alpha = .75) +
  scale_fill_discrete(guide = guide_legend(
                        direction = "horizontal",
                        title.position = "top"
                      )) +
  labs(fill = "Type") +
  theme_minimal(base_size=12) +
  theme(legend.position = c(.25,.75),
        # legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(fill = "grey95"),
        legend.text.align = 0,
        # legend.spacing.x = unit(.5,"cm"),
        panel.background = element_rect(color = "grey95")) +
  xlab("Supremum distance") +
  facet_grid(type2+n~d)






##################
res[, p := d*(d-1)/2]
res[loss_type == "maha", loss_p := loss - p]
res[loss_type == "maha2", loss_p := loss - (d-1)]
res[loss_type == "maha3", loss_p := loss - (p-d)]


ggplot(res[grep("maha",loss_type)][dtau == 0], aes(x=loss,fill=loss_type)) +
  geom_density(alpha = .75) +
  # geom_density(data = data.frame(loss = mc_sup, loss_type="mc"), aes(x=loss), alpha = .75) +
  scale_fill_discrete(guide = guide_legend(
    direction = "horizontal",
    title.position = "top"
  )) +
  labs(fill = "Type") +
  theme_minimal(base_size=12) +
  theme(legend.position = c(.25,.75),
        # legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(fill = "grey95"),
        legend.text.align = 0,
        # legend.spacing.x = unit(.5,"cm"),
        panel.background = element_rect(color = "grey95")) +
  xlab("Supremum distance") +
  facet_grid(tau+n~d)





# ggplot(res[loss_type == "maha" & dtau == 0,], aes(x = pchisq(loss, p-1, 4*p/n), fill = as.factor(tau))) +
ggplot(res[grep("maha",loss_type)][dtau == 0,], aes(x = loss, fill = loss_type, colour = as.factor(tau))) +
  geom_density(lwd = 1) +
  theme_minimal(base_size=12) +
  theme(panel.background = element_rect(color = "grey95")) +
  xlab("Mahalanobis distance") +
  facet_grid(n~d)



# res[grep("maha",loss_type), loss_chi := pchisq(loss, p-1, 4*(p/n), lower.tail = T)]
res[grep("maha",loss_type), loss_chi := pchisq(loss, p-1, lower.tail = F)]


ggplot(res[loss_type == "maha"][dtau == 0], aes(x=loss_chi, fill=loss_type)) +
  geom_histogram(alpha = .75, breaks = seq(0,1,.1)) +
  scale_fill_discrete(guide = guide_legend(
    direction = "horizontal",
    title.position = "top"
  )) +
  labs(fill = "Type") +
  theme_minimal(base_size=12) +
  theme(legend.position = c(.25,.75),
        # legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(fill = "grey95"),
        legend.text.align = 0,
        # legend.spacing.x = unit(.5,"cm"),
        panel.background = element_rect(color = "grey95")) +
  xlab("Mahalanobis distance") +
  facet_grid(tau+n~d)



my_p <- res[loss_type == "maha"][dtau == 0 & d == 20]$p[1]

ggplot(res[loss_type == "maha"][dtau == 0 & d == 20], aes(x=loss)) +
  geom_histogram(aes(y =..density..)) +
  theme_minimal(base_size=12) +
  xlab("Mahalanobis distance") +
  facet_grid(n~tau) +
  stat_function(fun = dchisq, args = list(df = my_row[1,]$p), col = "green", lwd = 1.1)





