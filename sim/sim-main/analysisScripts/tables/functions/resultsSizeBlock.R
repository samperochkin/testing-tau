resultsSizeBlock <- function(dt, dis){
  
  dt <- dt[distribution == dis]

  ## size - H0
  ns <- c(50,150,250)
  ds <- c(6,12,18)
  dta <- 0
  Shs <- c("ShP","ShJ")
  
  xdt <- dcast(dt[S == "I" &
                    n %in% c(50,150,250) & d %in% c(5,15) &
                    dtau == dta & Sh %in% Shs],
                  formula = Sh + d ~ norm + design + n,
                  value.var = "rejection_rate")

  R <- round(xdt[,-(1:2)],1)
  R
}