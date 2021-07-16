tableBlockWrapper <- function(dt, grid_line){
  
  distribution <- grid_line$distribution
  dtau <- grid_line$dtau
  Sh <- grid_line$Sh
  M <- grid_line$min_N
  
  table_type <- ""
  if(dtau == 0) table_type <- paste0(table_type, "Size")
  if(dtau != 0) table_type <- paste0(table_type, "Power")

  if(Sh == "Sb") table_type <- paste0(table_type, "Star")
  
  if(table_type == "Size"){
    R <- resultsSizeBlock(dt, distribution)
    return(tableSizeBlock(R, M, distribution))
  } 
  if(table_type == "SizeStar"){
    R <- resultsSizeStarBlock(dt, distribution)
    return(tableSizeStarBlock(R, M, distribution))
  } 

  if(table_type == "Power"){
    R <- resultsPowerBlock(dt, distribution, dtau)
    return(tablePowerBlock(R, M, distribution, dtau))
  }
  if(table_type == "PowerStar"){
    R <- resultsPowerStarBlock(dt, distribution, dtau)
    return(tablePowerStarBlock(R, M, distribution, dtau))
  }
}
