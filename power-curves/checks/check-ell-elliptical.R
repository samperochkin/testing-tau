n <- 100
d <- 5
p <- choose(d,2)
tau <- .3
dtau_type <- "single"

X <- matrix(rnorm(n*d),n,d)
if(dtau_type == "single") h <- c(1,rep(0,p-1))
if(dtau_type == "column") h <- c(rep(0,d-1),rep(1,p-d+1))
rho <- sin(tau*pi/2)
R <- diag(d) + (1-diag(d))*rho


#### NORMAL
scores <- scoreFunction(X, distribution = "normal", tau, departure = "single")

step_size <- .0001
s <- seq(rho-step_size, rho+step_size, step_size)
ll <- sapply(s, function(tt){
  if(dtau_type == "single") R[1,2] <- R[2,1] <- tt
  if(dtau_type == "column") R[1,2:d] <- R[2:d,1] <- tt
  sum(apply(X,1,function(x) mvtnorm::dmvnorm(x, rep(0,d), sigma = R, log = T)))
})

sum(scores %*% h)
mean(diff(ll))/step_size



#### T4
scores <- scoreFunction(X, distribution = "t4", tau, departure = "single")

step_size <- .0001
s <- seq(rho-step_size, rho+step_size, step_size)
ll <- sapply(s, function(tt){
  if(dtau_type == "single") R[1,2] <- R[2,1] <- tt
  if(dtau_type == "column") R[1,2:d] <- R[2:d,1] <- tt
  sum(apply(X,1,function(x) mvtnorm::dmvt(x, rep(0,d), sigma = R, df = 4, log = T)))
})

sum(scores %*% h)
mean(diff(ll))/step_size
