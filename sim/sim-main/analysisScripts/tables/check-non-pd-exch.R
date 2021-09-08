library(magrittr)
library(data.table)


# data --------------------------------------------------------------------
dt <- fread("sim/sim-main/results/dt_exch_full.csv")
dt$Sh <- factor(dt$Sh, levels = c("ShP", "ShJ", "SbP", "SbJ"))

dt_N <- dt[,unique(N),.(S,Sh,distribution)]
dt_N
dt_N <- dt[,.(min_N = min(N)),.(distribution)]

# table content -----------------------------------------------------------
distributions <- c("normal", "t4", "gumbel", "clayton")

lapply(distributions, function(dis){
  
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
                    value.var = "pd_rate")
  
  xdt.Sh.M <- dcast(dt[norm == "Supremum" & S == "Sh" &
                         n %in% c(50,150,250) & d %in% c(5,15) &
                         dtau == dta & dtau_type == dtau_t &
                         tau %in% c(0,.3,.6) & Sh %in% Shs],
                    formula = Sh + d ~ tau + n,
                    value.var = "pd_rate")
  
  rbind(xdt.Sh.E,xdt.Sh.M)
})

