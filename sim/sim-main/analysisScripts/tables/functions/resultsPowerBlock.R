resultsPowerBlock <- function(dt, dis, dta){
  
  dt <- dt[distribution == dis]

  ## size - H0
  ns <- c(50,150,250)
  ds <- c(6,12,18)
  Shs <- c("ShP","ShJ")
  
  xdt <- dcast(dt[S == "I" &
                    n %in% ns & d %in% ds &
                    dtau == dta & Sh %in% Shs],
                  formula = Sh + d ~ norm + design + n,
                  value.var = "rejection_rate")

  R <- round(xdt[,-(1:2)],1)
  R
}