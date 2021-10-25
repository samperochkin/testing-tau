source("thesis-application-3/example/example-matrices.R")
diag(Tau) <- 1
X <- mvtnorm::rmvnorm(n=500, sigma = sin(pi*Tau/2))


source("thesis-application-3/structureBuilder.R")
struct <- structureBuilder(X=X,
                 hclust_method = "complete",
                 alpha = .05,
                 M = 1000)



plot(struct$dend)
image(t(cor.fk(X)[18:1,]))
image(t(struct$Tau.tilde[18:1,]))

