library(data.table)
library(ggplot2)

res <- fread("clayton2.csv")
res <- melt(res, measure.vars = c("maha","maha2","maha3",
                                  "euc","euc2","euc3",
                                  "sup","sup2","sup3",
                                  "supS","supS2","supS3"), variable.name = "loss_type", value.name = "loss")

res[grep("sup",loss_type), type2 := "unstand"]
res[grep("supS",loss_type), type2 := "stand"]
res[grep("euc",loss_type), type2 := "unstand"]
res[grep("maha",loss_type), type2 := "stand"]


ffun <- function(nn = 200, dd = 5, tt = .3, dtt = 0){
  # sapply(c("sup","sup2","sup3"), function(ss){
  sapply(c("supS","supS2","supS3"), function(ss){
      c(
      res[loss_type == ss &
            n == nn & 
            tau == tt & 
            dtau == dtt &
            d == dd][order(num_sim)]$loss
    )
  })  
}

cor(ffun(dd = 5))
cor(ffun(dd = 10))
cor(ffun(dd = 15))
cor(ffun(dd = 20))


cor(ffun(dd = 5, dtt = .3))
cor(ffun(dd = 10, dtt = .3))
cor(ffun(dd = 15, dtt = .3))
cor(ffun(dd = 20, dtt = .3))



mc_sup2 <- replicate(1000, {max(abs(rnorm(p)))}) # choose p
mc_sup3 <- replicate(1000, {max(abs(rnorm(p)))}) # choose p

mc_supS <- replicate(1000, {max(abs(rnorm(p)))}) # choose p
mc_supS2 <- replicate(1000, {max(abs(rnorm(p)))}) # choose p
mc_supS3 <- replicate(1000, {max(abs(rnorm(p)))}) # choose p

