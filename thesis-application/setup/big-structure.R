
S1 <- list(
  createMatrix(4, "equi", .7),
  createMatrix(3, "un1", .8),
  createMatrix(2, "equi", .6),
  createMatrix(1, "equi", 1))
S1 <- nestMartix(createMatrix(4, "equi",.5), S1)

S2 <- list(
  createMatrix(4, "un1", .9,.35),
  createMatrix(3, "equi", .5))
S2 <- nestMartix(createMatrix(2, "equi",.35), S2)

S3 <- createMatrix(5,"un1",.75,.35)

S <- nestMartix(createMatrix(3, "un2",.35,.25), list(S1,S2,S3))

S4 <- createMatrix(5,"un2",.55,.25)
S5 <- createMatrix(1,"equi",1)

S <- nestMartix(createMatrix(6, "un2",.20), list(S,S1,S2,S3,S4,S5))

image(t(S[ncol(S):1,]))
