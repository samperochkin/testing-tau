createGrid <- function(n=100, d=10, tau=.5, distribution="normal", num_sim = 250){
  # simulation grid
  # same as simFun
  sim.grid <- as.data.table(
    expand.grid(
      n = n,
      d = d,
      tau = tau,
      dtau_type = c("none"),
      distribution = distribution,
      num_sim = 1:num_sim
    )
  )
    
  # add unique ids
  sim.grid$ID <- 1:nrow(sim.grid)
  return(sim.grid)
}
