designToClus <- function(d,design){
  if(design == "equi") return(rep(1,each=d))
  if(design == "block") return(rep(1:5,each=d/5))
}
