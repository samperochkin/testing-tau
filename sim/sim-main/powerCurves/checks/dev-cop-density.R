library(HAC)
d <- 5
theta <- 3
tree = list(list("X1", "X2", theta), "X3", "X4", "X5", theta)
model = hac(type = 7, tree = tree)

# sample from copula model
U = rHAC(3, model)

t <- 1.5
u <- .25

copula::copJoe@psi(t, theta)
copula::copJoe@iPsi(u, theta)

# psi <- function(t) copula::copJoe@psi(t, theta)
# iPsi <- function(u) copula::copJoe@iPsi(u, theta)
tu <- function(u) sum(copJoe@iPsi(u, theta))

copDensity <- function(U){
  
  d <- ncol(U)
  xx <- copJoe@absdPsi(copJoe@iPsi(pHAC(U, model), theta), theta, degree = d) *
    apply(copJoe@absdiPsi(U, theta),1,prod)
  sum(xx)
}

sum(dHAC(U, model))
copDensity(U)




# check the accurancy of the estimation procedure
ll = to.logLik(sample, model)
ll.value = to.logLik(sample, model, eval = TRUE)
sum(log(dHAC(sample, model)))

# sample from copula model
sample = rHAC(10, model)

fun <- function(tau){
  tree = list(list("X1", "X2", tau2theta(.35, 7)), list("X3", "X4", "X5", tau2theta(.35, 7)), tau2theta(.3, 7))
  model = hac(type = 7, tree = tree)
  
  # check the accurancy of the estimation procedure
  ll = to.logLik(sample, model)
  ll.value = to.logLik(sample, model, eval = TRUE)
  
}


ll(c(2, 3, 4)) == ll.value # [1] TRUE

library(HAC)

n <- 1
d <- 2
tau <- .3
dtau <- .1
hac.model <- hac(7,c(as.list(as.character(1:d)),tau2theta(tau,7)))
tree <- c(list(c(as.list(paste0(1:2)),tau2theta(tau+dtau,7))),c(as.list(paste0(3:d)),tau2theta(tau,7)))
hac.model <- hac(7,tree)
U <- rHAC(n, hac.model)


to.logLik(U, hac.model, eval = TRUE, margins = "unif", sum.log = TRUE)

hac.fun <- dHAC(U, hac.model, eval = F)
hac.fun(U)

HAC:::.cop.pdf(tree = hac.model$tree, sample = U, type = 7, d = d, names = as.character(1:d), eval = T)

string.expr = HAC:::.constr.expr(tree, 7)
Dd = HAC:::.d.dell(parse(text = string.expr), as.character(1:d), d)



X <- rHAC(n, hac(7,c(as.list(as.character(1:d)),tau2theta(tau,7))))
}else if(dtau_type == "single"){
  tree <- c(list(c(as.list(paste0(1:2)),tau2theta(tau+dtau,7))),c(as.list(paste0(3:d)),tau2theta(tau,7)))
  X <- rHAC(n, hac(7,tree))
}else if(dtau_type == "column"){
  tree <- list("1", c(as.list(paste0(2:d)),tau2theta(tau+dtau,7)), tau2theta(tau,7))
  X <- rHAC(n, hac(7,tree))
}


numDeriv::grad()


