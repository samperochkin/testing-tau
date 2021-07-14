tableWrapper <- function(dt, grid_line){
  
  distribution <- grid.line$distribution
  dtau_type <- grid.line$dtau_type
  dtau <- grid.line$dtau
  Sh <- grid.line$Sh
  
  table_type <- ""
  if(dtau_type == "none") table_type <- paste0(table_type, "Size")
  if(dtau_type != "none") table_type <- paste0(table_type, "Power")

  if(Sh == "Sb") table_type <- paste0(table_type, "Star")
  
  if(table_type == "Size"){
    R <- resultsSize(dt, distribution)
    tableSize(R, M, distribution)
  } 
  if(table_type == "SizeStar"){
    R <- resultsSizeStar(dt, distribution)
    tableSize(R, M, distribution)
  } 

  if(table_type == "Power"){
    R <- resultsPower(dt, distribution, dtau)
    tablePower(R, M, distribution, dtau)
  }
  if(table_type == "PowerStar"){
    R <- resultsPowerStar(dt, distribution, dtau, dtau_type)
    tablePowerStar(R, M, distribution, dtau, dtau_type)
  }
}
