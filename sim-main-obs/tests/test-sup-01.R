G <- B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shi
I <- diag(p)
IG <- I - G
SIG <- Shih %*% IG

SIG %*% Sh %*% t(SIG)


(
I - Shih %*% B %*% solve(t(B) %*% Shi %*% B) %*% t(B) %*% Shih
-  
SIG %*% Sh %*% t(SIG)
)  


(
  I - (Shih %*% B) %*% ginv(Shih %*% B)
  -  
  SIG %*% Sh %*% t(SIG)
)  

diag(t(I - G))
diag(SIG %*% Sh %*% t(SIG))


plot(diag(t(I - G)),
     diag(SIG %*% Sh %*% t(SIG)))
lines(c(-1,1),c(-1,1), col="red")

sum(abs(t(I - G) - SIG %*% Sh %*% t(SIG)))

plot(diag(t(I - B %*% ginv(B))),
     diag(SIG %*% Sh %*% t(SIG)))
lines(c(-1,1),c(-1,1), col="red")


eig <- eigen(Sh)
V <- eig$vectors
D <- diag(eig$values)

V %*% D %*% t(V)
sum(abs(V %*% D %*% t(V) - Sh))

I - Shih %*% G %*% Shh

Shih %*% G %*% Shh
B %*% ginv(B) %*% ginv(G)


B %*% ginv(B) %*% (I - G)


I - G %*% ginv(G) %*% G