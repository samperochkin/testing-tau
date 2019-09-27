# source("simFun.R")
# simFun(n = 200, d = 10, k = NULL, tau = .5, dtau = 0, distribution = "joe", num_sim = 1000, filename = "joe1")

library(data.table)
library(ggplot2)

res <- fread("joe1.csv")
res <- melt(res, measure.vars = c("maha","maha2","maha3","euc","euc2","euc3"), variable.name = "loss_type", value.name = "loss")

ggplot(res[grep("maha",loss_type),], aes(x=loss,fill=loss_type)) +
  geom_density(alpha = .5) +
  stat_function(fun = dchisq, args = list(df = 44), col = "red", lwd = 1, lty = 2) +
  stat_function(fun = dchisq, args = list(df = 35), col = "blue", lwd = 1, lty = 2) +
  stat_function(fun = dchisq, args = list(df = 9), col = "green", lwd = 1, lty = 2) +
  # stat_function(fun = dchisq, args = list(df = 44), aes(colour = "red"), lwd = 1, lty = 2) +
  # stat_function(fun = dchisq, args = list(df = 35), aes(col = "blue"), lwd = 1, lty = 2) +
  # stat_function(fun = dchisq, args = list(df = 9), aes(col = "green"), lwd = 1, lty = 2) +
  # scale_colour_manual(name="Asymptotic distribution",
  #                     labels = c(expression(chi[p-1]^2,chi[d-1]^2,chi[p-d]^2)),
  #                     values=c(red="red", blue="blue", green="green")) +
  scale_fill_discrete(labels=c(expression("("*delta[2]*","*delta[3]*")      "),expression(delta[2]*"      "),expression(delta[3]*"      ")),
                      guide = guide_legend(
                        direction = "horizontal",
                        title.position = "top"
                      )) +
  labs(fill = "Associated eigenvalues") +
  theme_minimal(base_size=12) +
  theme(legend.position = c(.6,.75),
        # legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(fill = "grey95"),
        legend.text.align = 0,
        # legend.spacing.x = unit(.5,"cm"),
        panel.background = element_rect(color = "grey95")) +
  xlab("Mahalanobis distance")



# ggplot(res[grep("euc",loss_type),], aes(x=loss,fill=loss_type)) +
#   geom_density(alpha = .5)
