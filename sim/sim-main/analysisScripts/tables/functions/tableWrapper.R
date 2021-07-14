tableWrapper <- function(dt, grid_line){
  
  distribution <- grid_line$distribution
  dtau_type <- grid_line$dtau_type
  dtau <- grid_line$dtau
  Sh <- grid_line$Sh
  M <- grid_line$min_N
  
  table_type <- ""
  if(dtau_type == "none") table_type <- paste0(table_type, "Size")
  if(dtau_type != "none") table_type <- paste0(table_type, "Power")

  if(Sh == "Sb") table_type <- paste0(table_type, "Star")
  
  if(table_type == "Size"){
    R <- resultsSize(dt, distribution)
    return(tableSize(R, M, distribution))
  } 
  if(table_type == "SizeStar"){
    R <- resultsSizeStar(dt, distribution)
    return(tableSizeStar(R, M, distribution))
  } 

  if(table_type == "Power"){
    R <- resultsPower(dt, distribution, dtau)
    return(tablePower(R, M, distribution, dtau))
  }
  if(table_type == "PowerStar"){
    R <- resultsPowerStar(dt, distribution, dtau, dtau_type)
    return(tablePowerStar(R, M, distribution, dtau, dtau_type))
  }
}
