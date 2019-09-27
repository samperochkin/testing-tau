library(data.table)
library(ggplot2)

res <- fread("clayton2.csv")
res <- melt(res, measure.vars = c("maha","maha2","maha3",
                                  "euc","euc2","euc3",
                                  "sup","sup2","sup3",
                                  "supS","supS2","supS3"), variable.name = "loss_type", value.name = "loss")

res[grep("sup",loss_type), type2 := "unstand"]
res[grep("supS",loss_type), type2 := "stand"]

#mc_sup <- replicate(1000, {max(abs(rnorm(p)))}) # choose p

ggplot(res[grep("sup",loss_type)][dtau == 0 & tau == .25], aes(x=loss,fill=loss_type)) +
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
        panel.background = element_rect(color = "grey95"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab("Supremum distance") +
  facet_grid(type2+n~d)


res$loss_type <- factor(res$loss_type, levels = c(
  "maha","maha2","maha3",
  "euc","euc2","euc3",
  "sup","supS","sup2","supS2","sup3","supS3"))

res$type2 <- factor(res$type2, levels = c("unstand","stand"))

ggplot(res[grep("sup",loss_type)][dtau == 0 & tau == .25 & n == 100], aes(x=loss,fill=loss_type)) +
  geom_density(alpha = .75) +
  # geom_density(data = data.frame(loss = mc_sup, loss_type="mc"), aes(x=loss), alpha = .75) +
  scale_fill_discrete(guide = guide_legend(
    direction = "horizontal",
    title.position = "top"
  )) +
  labs(fill = expression("Type (n=100, "*tau*"=0.25)")) +
  theme_minimal(base_size=14) +
  theme(legend.position = c(.15,.40),
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(fill = "grey95"),
        legend.text.align = 0,
        # legend.spacing.x = unit(.5,"cm"),
        panel.background = element_rect(color = "grey95"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlab("Supremum distance") +
  facet_grid(type2+n~d) +
  theme(strip.text.y = element_blank())

#