pvals <- replicate(1000, {
  
  X <- generateData(n = 200,
                    d = 50,
                    tau = .3,
                    dtau = 0,
                    #dtau_type = "column",
                    distribution = "joe")
  
  testEquiRank(X, 500)
})

sum(abs(sort(pvals)-(1:1000)/1001))
lines(sort(pvals), ylim = c(0,1), col="orange")


plot(pvals, ylim = c(0,1))
hist(pvals)




tt <- (colSums(Tau.hat)-1)/(d-1)

t(tt-tau.bar) %*% as.matrix(t(B) %*% Sh %*% B) %*% (tt-tau.bar)

B %*% (tt-tau.bar) - (tt[l.ij.mat[,1]] + tt[l.ij.mat[,2]] - 2*tau.bar)


(tt-tau.bar) %*% as.matrix(t(B) %*% Sh %*% B) %*% (tt-tau.bar)


sum((tt[l.ij.mat[,1]] + tt[l.ij.mat[,2]] - 2*tau.bar)^2) -
  (sum((tt[l.ij.mat[,1]] - tau.bar)^2) + sum((tt[l.ij.mat[,2]] - tau.bar)^2)) -
  2*crossprod(tt[l.ij.mat[,1]] - tau.bar,tt[l.ij.mat[,2]] - tau.bar)

     