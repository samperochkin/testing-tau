resultsSize <- function(dt, dis){
  
  dt <- dt[distribution == dis]

  ## size - H0
  ns <- c(50,100,150)
  ds <- c(5,15,25,50,100)
  dta <- 0
  dtau_t <- "none"
  Shs <- c("ShP","ShJ")
  
  xdt.Sh.E <- dcast(dt[norm == "Euclidean" & S == "Sh" &
                          n %in% c(50,150,250) & d %in% c(5,15) &
                          dtau == dta & dtau_type == dtau_t &
                          tau %in% c(0,.3,.6) & Sh %in% Shs],
                    formula = Sh + d ~ tau + n,
                    value.var = "rejection_rate")
  
  xdt.Sh.M <- dcast(dt[norm == "Supremum" & S == "Sh" &
                          n %in% c(50,150,250) & d %in% c(5,15) &
                          dtau == dta & dtau_type == dtau_t &
                          tau %in% c(0,.3,.6) & Sh %in% Shs],
                    formula = Sh + d ~ tau + n,
                    value.var = "rejection_rate")
  
  xdt.I.E <- dcast(dt[norm == "Euclidean" & S == "I" &
                         n %in% ns & d %in% ds &
                         dtau == dta & dtau_type == dtau_t &
                         tau %in% c(0,.3,.6) & Sh %in% Shs],
                   formula = Sh + d ~ tau + n,
                   value.var = "rejection_rate")
  
  xdt.I.M <- dcast(dt[norm == "Supremum" & S == "I" &
                         n %in% ns & d %in% ds &
                         dtau == dta & dtau_type == dtau_t &
                         tau %in% c(0,.3,.6) & Sh %in% Shs],
                   formula = Sh + d ~ tau + n,
                   value.var = "rejection_rate")
  
  
  # because not all tables have same colnames...
  names(xdt.Sh.E) <- names(xdt.Sh.M) <- names(xdt.I.E) <- names(xdt.I.M) <- as.character(1:ncol(xdt.I.M))
  
  R <- round(rbind(xdt.Sh.E, xdt.Sh.M, xdt.I.E, xdt.I.M)[,-(1:2)],1)
  if(dis == "clayton") R <- cbind(0,0,0,R)
  R
}