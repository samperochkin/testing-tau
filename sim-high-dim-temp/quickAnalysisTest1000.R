# res <- readRDS("res-test.rds")
# res <- readRDS("res-test2.rds")
res <- readRDS("res-testclay.rds")

plot(res[2,])
hist(res[2,])

mean(res[1,])


d <- 500
p <- d*(d-1)/2

hist(res[1,], probability = T, breaks=25)
ss <- seq(min(res[1,]),max(res[1,]),1)
lines(ss,dchisq(ss,p-1))

qqnorm((res[1,]-(p-1))/sqrt(2*(p-1)))
lines(matrix(c(-5,5,-5,5),2), col="red")

qqplot(res[1,], rchisq(10000, p-1))
lines(matrix(c(min(res[1,]),max(res[1,]),min(res[1,]),max(res[1,])),2), col="red")


quantile(res[2,], seq(0,1,.025))
quantile(pchisq(res[1,],p-1), seq(0,1,.025))


plot(seq(0,1,.025), quantile(res[2,], seq(0,1,.025)) - seq(0,1,.025), type="l")
lines(seq(0,1,.025), quantile(pchisq(res[1,],p-1,lower.tail = F), seq(0,1,.025)) - seq(0,1,.025), col="red")


# hist((res[1,]-(p-1))/sqrt(2*(p-1)), breaks=25, prob=T)
# ss <- seq(-5,5,.01)
# lines(ss,dnorm(ss))

mean(res[2,] < .05)
mean(res[2,] < .01)

