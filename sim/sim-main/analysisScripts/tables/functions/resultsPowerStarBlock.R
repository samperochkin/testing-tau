resultsPowerStarBlock <- function(dt, dis){
  
  dt <- dt[distribution == dis]

  ## size - H0
  ns <- c(50,150,250)
  ds <- c(6,12,18)
  dta <- .1
  Shs <- c("SbP","SbJ")
  
  xdt.Sh <- dcast(dt[S == "Sh" &
                    n %in% c(50,150,250) & d %in% c(5,15) &
                    dtau == dta & Sh %in% Shs],
                  formula = Sh + d ~ norm + design + n,
                  value.var = "rejection_rate")

  xdt.I <- dcast(dt[S == "I" &
                    n %in% c(50,150,250) & d %in% c(5,15) &
                    dtau == dta & Sh %in% Shs],
               formula = Sh + d ~ norm + design + n,
               value.var = "rejection_rate")

  R <- round(rbind(xdt.Sh,xdt.I)[,-(1:2)],1)
  R
}