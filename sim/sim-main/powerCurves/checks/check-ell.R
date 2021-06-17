R <- cor(matrix(runif(10*50),50,10))


# verify derivative of log-det
fun <- function(x){
  R.temp <- R
  R.temp[1,2] <- R.temp[2,1] <- R.temp[1,2] + x
  log(det(R.temp))
}


ss <- sapply(seq(-.001,.001,.0001), fun)
mean(diff(ss)[10:11])/.0001
2*solve(R)[1,2]


# verify derivative of inverse
v <- runif(10)

fun <- function(x){
  R.temp <- R
  R.temp[1,2] <- R.temp[2,1] <- R.temp[1,2] + x
  t(v) %*% solve(R.temp) %*% v
}


ss <- sapply(seq(-.001,.001,.0001), fun)
mean(diff(ss)[10:11])/.0001

E <- matrix(0,10,10)
E[1,2] <- E[2,1] <- 1
Ri <- solve(R)
- t(v) %*% Ri %*% E %*% Ri %*% v

