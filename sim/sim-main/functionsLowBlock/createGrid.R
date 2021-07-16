createGrid <- function(n=100, d=10, design="balanced", dtau=0, distribution="normal", num_sim = 250){
  # simulation grid
  if(identical(dtau,0)){
    
    # same as simFun
    sim.grid <- as.data.table(
      expand.grid(
        n = n,
        d = d,
        design = design,
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
        design = design,
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
      design = design,
      dtau = dtau[dtau != 0],
      dtau_type = c("single"),
      distribution = distribution,
      num_sim = 1:num_sim
    )
    
    # rbind grids
    sim.grid <- as.data.table(
      rbind(sim.grid0,sim.grid1)
    )
  }
  
  undefined <- which(sim.grid$tau == 0 & sim.grid$distribution %in% c("clayton","frank"))
  if(length(undefined) > 0) sim.grid <- sim.grid[-undefined,]
  
  
  # add unique ids
  sim.grid$ID <- 1:nrow(sim.grid)
  return(sim.grid)
}
