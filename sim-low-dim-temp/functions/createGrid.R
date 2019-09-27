createGrid <- function(n=100, d=10, tau=.5, dtau=0, distribution="normal", num_sim = 250){
  # simulation grid
  if(identical(dtau,0)){
    
    # same as simFun
    sim.grid <- as.data.table(
      expand.grid(
        n = n,
        d = d,
        tau = tau,
        dtau = dtau,
        distribution = distribution,
        num_sim = 1:num_sim
      )
    )
    
  }else{
    
    # grid for dtau == 0
    if(!any(dtau==0)){
      sim.grid0 <- NULL
    }else{
      sim.grid0 <- expand.grid(
        n = n,
        d = d,
        tau = tau,
        dtau = 0,
        dtau_type = c("none"),
        distribution = distribution,
        num_sim = 1:num_sim
      )  
    }
    
    # grid for dtau != 0
    sim.grid1 <- expand.grid(
      n = n,
      d = d,
      tau = tau,
      dtau = dtau[dtau != 0],
      dtau_type = c("single","column"),
      distribution = distribution,
      num_sim = 1:num_sim
    )
    
    # rbind grids
    sim.grid <- as.data.table(
      rbind(sim.grid0,sim.grid1)
    )
  }
  
  
  
  # add unique ids
  sim.grid$ID <- 1:nrow(sim.grid)
  return(sim.grid)
}
