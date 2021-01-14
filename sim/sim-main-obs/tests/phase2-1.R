M <- 10000
Z <- matrix(rnorm(M*(p-3)),M,p-3)

YU <- tcrossprod(Z,U)
YV <- tcrossprod(Z,V)

# x <- sort(apply(abs(YU),1,max))
# y <- sort(apply(abs(YV),1,max))

x <- sort(apply(abs(YU),1,sort)[4,])
y <- sort(apply(abs(YV),1,sort)[4,])
# x <- sort(apply(YU,1,sort)[4,])
# y <- sort(apply(YV,1,sort)[4,]) 

# a <- 4/p*(p-L)
# al <- floor(a)
# au <- ceiling(a)
# w <- c(au-a,a-al)
# a <- w %*% c(al,au)
# z <- sort(t(w) %*% apply(Z,1,sort)[c(au,al),])
  

# rm(Z)
# rm(YU)
# rm(YV)

pp <- seq(0,1,.01)
# x <- quantile(x, probs=pp)
# y <- quantile(y, probs=pp)

par(mar=c(2,2,1,1))
ran <- range(c(quantile(x,probs=c(.01,.99)),quantile(y,probs=c(.01,.99))))
sum(abs(quantile(x,probs=c(.99))-quantile(y,probs=c(.99))))
# plot(x,y, cex=1)
plot(x,y, type="l", xlim = ran + c(-1,1)*.01, ylim = ran + c(-1,1)*.01)
# lines(x,y,col="blue")
lines(c(-10,10),c(-10,10),col="red")
# abline(h = x[c("1.0%","99.0%")], lty=2)
# abline(v = x[c("1.0%","99.0%")], lty=2)
abline(v = quantile(x,probs=c(.01,.99)), lty=2)
abline(h = quantile(y,probs=c(.01,.99)), lty=2)
# # 
# # plot(quantile(x-y,probs=pp))
# # plot(quantile(x-y,probs=seq(.01,.99,.01)))


ll <- .1
plot(x,y, type="l", xlim = ran[2] + c(-1,1)*ll, ylim = ran[2]  + c(-1,1)*ll)
lines(c(-10,10),c(-10,10),col="red")
abline(v = quantile(x,probs=c(.01,.99)), lty=2)
abline(h = quantile(y,probs=c(.01,.99)), lty=2)
#


ll <- .1
plot(x,y, type="l", xlim = ran[1] + c(-1,1)*ll, ylim = ran[1]  + c(-1,1)*ll)
lines(c(-10,10),c(-10,10),col="red")
abline(v = quantile(x,probs=c(.01,.99)), lty=2)
abline(h = quantile(y,probs=c(.01,.99)), lty=2)



ll <- .1
plot(x,y, type="l", xlim = mean(x) + c(-1,1)*ll, ylim = mean(y)  + c(-1,1)*ll)
lines(c(-10,10),c(-10,10),col="red")
abline(v = quantile(x,probs=c(.01,.99)), lty=2)
abline(h = quantile(y,probs=c(.01,.99)), lty=2)







mean(x-y)
mean((x-y) > 0)

L <- ncol(B)
fuu <- function(a){
  # A <- matrix(0,p-L,p-L)
  # A[upper.tri(A)] <- a
  # A[lower.tri(A)] <- a
  # diag(A) <- 1-rowSums(A^2)
  # z1 <- rnorm(p-L)
  # z2 <- rnorm(p-L)
  A <- matrix(a,p-L,p-L)
  sum((U %*% A %*% Ui - V %*% Vi)^2)
}

(Ui %*% V %*% Vi %*% U) %*% solve(Ui %*% V %*% Vi %*% U)

solve(Ui %*% V %*% Vi %*% U) %*% solve(Ui %*% V %*% Vi %*% U)

P <- Ui %*% V %*% Vi
P <- P %*% ginv(P)
P %*% P - P

eigen(P)$val

# optim(par = rep(0,(p-L)*(p-L-1)/2),fuu)
op <- optim(par = rep(0,(p-L)^2),fuu)
op
a <- op$par

A <- matrix(a,p-L)
U %*% A - V      

image(t(A[nrow(A):1,]))
