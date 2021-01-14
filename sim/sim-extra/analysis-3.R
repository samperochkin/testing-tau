pvals <- readRDS("pvals_extra_3.rds")

par(mfrow=c(1,1), mar=c(2,2,1,1))
plot(seq_along(pvals)/length(pvals),sort(pvals),ylim=c(0,1))
