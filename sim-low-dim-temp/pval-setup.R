library(parallel)
library(data.table)
library(Matrix)


res <- fread("clayton2.csv")
res <- melt(res, measure.vars = c("maha","maha2","maha3",
                                  "euc","euc2","euc3",
                                  "sup","sup2","sup3",
                                  "supS","supS2","supS3"), variable.name = "loss_type", value.name = "loss")
res[, p := d*(d-1)/2]
res[, delta2 := sigma2 + (d-4)*sigma1 - (d-3)*sigma0]
res[, delta3 := sigma2 - 2*sigma1 + sigma0]

