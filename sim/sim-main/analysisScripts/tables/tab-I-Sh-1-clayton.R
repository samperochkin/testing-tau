library(data.table)
library(xtable)
# library(ggplot2)

# fii <- list.files("sim-main/results/",full.names = T)
# fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)
fii <- list.files("/store/samuel/testing-tau-extra",full.names = T)

ii <- grep("hac", fii)
dt <- rbindlist(lapply(fii[ii], fread), fill=T)
dt <- dt[distribution == "clayton"]

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

dt <- dt[S %in% c("Sh","I")]
#### À voir
#dt[isShPd == F, pvalue := NA]

# dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)),
             N = .N, mean_PSD = mean(isShPsd)),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type","distribution")]

dt2[mean_PSD < 1 & S %in% "Sh"]
dt2[mean_PSD < 1 & S %in% "I"]

dt2[mean_PSD < 1 & S %in% "Sh" & dtau == 0]
dt2[mean_PSD < 1 & S %in% "I" & dtau == 0]

addSpacings <- function(xdt, ks){
  ks <- c(0,ks,ncol(xdt))
  blocks <- lapply(seq_along(ks[-1]), function(i){
    xdt[,(ks[i]+1):ks[i+1]]
  })
  blocks2 <- as.list(rep(NA,2*length(blocks)-1))
  blocks2[seq(1,length(blocks2),2)] <- blocks
  cbind.data.frame(blocks2)
}
NAbind <- function(...) {
  lst <- list(...)
  lst <- lapply(lst, as.data.frame)
  bb <- do.call(
    rbind, 
    lapply(lst, function(x) {
      mat <- matrix(NA, ncol=ncol(x))
      colnames(mat) <- names(x)
      rbind(x, mat)
    }))
  as.data.table(bb)
}



## size - H0
ns <- c(50,100,150)
ds <- c(5,15,25,50,100)
dta <- 0
dtau_t <- "none"
Shs <- c("ShP","ShJ")

xdt.Sh.E <- dcast(dt2[norm == "Euclidean" & S == "Sh" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == dtau_t &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")

xdt.Sh.M <- dcast(dt2[norm == "Supremum" & S == "Sh" &
                       n %in% ns & d %in% ds &
                        dtau == dta & dtau_type == dtau_t &
                        tau %in% c(0,.3,.6) & Sh %in% Shs],
                 formula = Sh + d ~ tau + n,
                 value.var = "level")

xdt.I.E <- dcast(dt2[norm == "Euclidean" & S == "I" &
                       n %in% ns & d %in% ds &
                       dtau == dta & dtau_type == dtau_t &
                       tau %in% c(0,.3,.6) & Sh %in% Shs],
                 formula = Sh + d ~ tau + n,
                 value.var = "level")

xdt.I.M <- dcast(dt2[norm == "Supremum" & S == "I" &
                       n %in% ns & d %in% ds &
                       dtau == dta & dtau_type == dtau_t &
                       tau %in% c(0,.3,.6) & Sh %in% Shs],
                 formula = Sh + d ~ tau + n,
                 value.var = "level")

R <- round(rbind(xdt.Sh.E, xdt.Sh.M, xdt.I.E, xdt.I.M)[,-(1:2)],1)

xdt <- NAbind(xdt.Sh.E, xdt.Sh.M, xdt.I.E, xdt.I.M) # check
xdt <- xdt[,-c(1,2)]
xdt <- addSpacings(xdt, 3)
print(xtable(xdt,digits=1),include.rownames = F)



## size - H0^*
ns <- c(50,100,150)
ds <- c(5,15,25,50,100)
dta <- 0
dtau_t <- "none"
Shs <- c("SbP","SbJ")

xdt.Sh.E <- dcast(dt2[norm == "Euclidean" & S == "Sh" &
                        n %in% ns & d %in% ds &
                        dtau == dta & dtau_type == dtau_t &
                        tau %in% c(0,.3,.6) & Sh %in% Shs],
                  formula = Sh + d ~ tau + n,
                  value.var = "level")

xdt.Sh.M <- dcast(dt2[norm == "Supremum" & S == "Sh" &
                        n %in% ns & d %in% ds &
                        dtau == dta & dtau_type == dtau_t &
                        tau %in% c(0,.3,.6) & Sh %in% Shs],
                  formula = Sh + d ~ tau + n,
                  value.var = "level")

xdt.I.E <- dcast(dt2[norm == "Euclidean" & S == "I" &
                       n %in% ns & d %in% ds &
                       dtau == dta & dtau_type == dtau_t &
                       tau %in% c(0,.3,.6) & Sh %in% Shs],
                 formula = Sh + d ~ tau + n,
                 value.var = "level")

xdt.I.M <- dcast(dt2[norm == "Supremum" & S == "I" &
                       n %in% ns & d %in% ds &
                       dtau == dta & dtau_type == dtau_t &
                       tau %in% c(0,.3,.6) & Sh %in% Shs],
                 formula = Sh + d ~ tau + n,
                 value.var = "level")

xdt <- NAbind(xdt.Sh.E, xdt.Sh.M, xdt.I.E, xdt.I.M) # check
xdt <- xdt[,-c(1,2)]
xdt <- addSpacings(xdt, 3)
print(xtable(xdt,digits=1),include.rownames = F)




#### HERE ---------

# departures....
dta <- .1
xdt1 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == "single" &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")

xdt2 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == "single" &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")
## column
xdt3 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == "column" &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")
xdt4 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == "column" &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")




dt <- rbindlist(lapply(fii[c(1,2)],fread))
dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)), "NN" = .N),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type")]

ds <- c(50,100)

xdt12 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                     n %in% ns & d %in% ds &
                     dtau == dta & dtau_type == "single" &
                     tau %in% c(0,.3,.6) & Sh %in% Shs],
               formula = Sh + d ~ tau + n,
               value.var = "level")

xdt22 <- dcast(dt2[norm == "Supremum" & S == "I" &
                     n %in% ns & d %in% ds &
                     dtau == dta & dtau_type == "single" &
                     tau %in% c(0,.3,.6) & Sh %in% Shs],
               formula = Sh + d ~ tau + n,
               value.var = "level")

xdt32 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                     n %in% ns & d %in% ds &
                     dtau == dta & dtau_type == "column" &
                     tau %in% c(0,.3,.6) & Sh %in% Shs],
               formula = Sh + d ~ tau + n,
               value.var = "level")

xdt42 <- dcast(dt2[norm == "Supremum" & S == "I" &
                     n %in% ns & d %in% ds &
                     dtau == dta & dtau_type == "column" &
                     tau %in% c(0,.3,.6) & Sh %in% Shs],
               formula = Sh + d ~ tau + n,
               value.var = "level")


xdt <- rbind(xdt1,xdt12,xdt2,xdt22,xdt3,xdt32,xdt4,xdt42)
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)


xdt <- rbind(xdt1,xdt12,xdt2,xdt22)
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)


xdt <- rbind(xdt1,xdt12,xdt2,xdt22)
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
