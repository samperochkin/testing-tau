S1 <- createMatrix(4, "equi", .7)
S1 <- nestMartix(S1, list(createMatrix(2,"equi",.8)))


S1 <- list(
  S1,
  createMatrix(3, "un1", .8),
  createMatrix(2, "equi", .6),
  createMatrix(1, "equi", 1))
S1 <- nestMartix(createMatrix(4, "un1",.5), S1)

S2 <- list(
  createMatrix(4, "un1", .9,.35),
  createMatrix(3, "equi", .5))
S2 <- nestMartix(createMatrix(2, "equi",.3), S2)

S3 <- createMatrix(5,"un2",.55,.3)
S4 <- createMatrix(2,"equi",.35)
S5 <- createMatrix(2,"equi",.25)

S <- nestMartix(createMatrix(3, "un1",.25,.15), list(S1,S2,S3))
S <- nestMartix(createMatrix(3, "equi",0), list(S1,S4,S5))

image(t(S[ncol(S):1,]))


