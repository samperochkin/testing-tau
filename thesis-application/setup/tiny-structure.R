S <- createMatrix(2, "equi", .7)
S <- nestMartix(S, list(createMatrix(2,"equi",.8)))

S <- list(
  S,
  createMatrix(3, "un1", .8),
  createMatrix(3, "equi", .6))

S <- nestMartix(createMatrix(3, "un1",.5), S)


image(t(S[ncol(S):1,]))


