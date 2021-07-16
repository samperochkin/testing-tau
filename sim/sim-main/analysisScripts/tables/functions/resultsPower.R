resultsPower <- function(dt, dis, dta){
  
  dt <- dt[distribution == dis]
  
  ## size - H0
  ns <- c(50,100,150)
  ds <- c(5,15,25,50,100)
  Shs <- c("ShP","ShJ")
  
  xdt.I.E.s <- dcast(dt[norm == "Euclidean" & S == "I" &
                          n %in% ns & d %in% ds &
                         dtau == dta & dtau_type == "single" &
                         tau %in% c(0,.3,.6) & Sh %in% Shs],
                    formula = Sh + d ~ tau + n,
                    value.var = "rejection_rate")
  
  xdt.I.M.s <- dcast(dt[norm == "Supremum" & S == "I" &
                          n %in% ns & d %in% ds &
                         dtau == dta & dtau_type == "single" &
                         tau %in% c(0,.3,.6) & Sh %in% Shs],
                    formula = Sh + d ~ tau + n,
                    value.var = "rejection_rate")
  
  xdt.I.E.c <- dcast(dt[norm == "Euclidean" & S == "I" &
                         n %in% ns & d %in% ds &
                        dtau == dta & dtau_type == "column" &
                        tau %in% c(0,.3,.6) & Sh %in% Shs],
                   formula = Sh + d ~ tau + n,
                   value.var = "rejection_rate")
  
  xdt.I.M.c <- dcast(dt[norm == "Supremum" & S == "I" &
                         n %in% ns & d %in% ds &
                        dtau == dta & dtau_type == "column" &
                        tau %in% c(0,.3,.6) & Sh %in% Shs],
                   formula = Sh + d ~ tau + n,
                   value.var = "rejection_rate")
  
  R <- round(rbind(xdt.I.E.s, xdt.I.M.s, xdt.I.E.c, xdt.I.M.c)[,-(1:2)],1)
  if(dis == "clayton") R <- cbind(0,0,0,R)
  R
}