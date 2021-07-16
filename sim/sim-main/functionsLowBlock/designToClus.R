designToClus <- function(d,design){
  if(design == "balanced") return(rep(1:3,each=d/3))
  if(design == "unbalanced") return(unlist(sapply(1:3, function(k) rep(k,each=d*(4-k)/6))))
}
