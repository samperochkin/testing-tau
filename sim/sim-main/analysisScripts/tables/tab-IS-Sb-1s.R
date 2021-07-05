ns <- c(50,100,150)
ds <- c(5,15,25)

dta <- .1
dtt <- "single"
Shs <- c("SbP","SbJ")

# fii <- list.files("sim-main/results/",full.names = T)
fii <- list.files("C:/Users/Samuel/Gits/tt/",full.names = T)

dt <- rbindlist(lapply(fii[6:7],fread))

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

# dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = .N),
dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue))),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type")]

## single
xdt1 <- dcast(dt2[norm == "Euclidean" & S == "Sh" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == dtt &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")
xdt2 <- dcast(dt2[norm == "Supremum" & S == "Sh" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == dtt &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")
xdt3 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == dtt &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")
xdt4 <- dcast(dt2[norm == "Supremum" & S == "I" &
                    n %in% ns & d %in% ds &
                    dtau == dta & dtau_type == dtt &
                    tau %in% c(0,.3,.6) & Sh %in% Shs],
              formula = Sh + d ~ tau + n,
              value.var = "level")



dt <- rbindlist(lapply(fii[3:5],fread))
all(dt$isShPd)

dt$dtau_type <- factor(dt$dtau_type, level = c("none","single","column"))
dt[,decision := pvalue < .05]

dt$dtau_type <- factor(dt$dtau_type,levels = c("none","single","column"))
dt$S <- factor(dt$S,levels = c("Sh","I","Sh-p","Sh-d"))
dt$Sh <- factor(dt$Sh,levels = c("ShP","ShJ","SbP","SbJ"))

dt2 <- dt[,.("level" = 100*mean(decision,na.rm=T), "N" = mean(!is.na(pvalue)), "NN" = .N),
          by=c("n","d","tau","S","Sh","norm","dtau","dtau_type")]

ds <- c(50,100)

xdt12 <- dcast(dt2[norm == "Euclidean" & S == "Sh" &
                     n %in% ns & d %in% ds &
                     dtau == dta & dtau_type == dtt &
                     tau %in% c(0,.3,.6) & Sh %in% Shs],
               formula = Sh + d ~ tau + n,
               value.var = "level")

xdt22 <- dcast(dt2[norm == "Supremum" & S == "Sh" &
                     n %in% ns & d %in% ds &
                     dtau == dta & dtau_type == dtt &
                     tau %in% c(0,.3,.6) & Sh %in% Shs],
               formula = Sh + d ~ tau + n,
               value.var = "level")

xdt32 <- dcast(dt2[norm == "Euclidean" & S == "I" &
                     n %in% ns & d %in% ds &
                     dtau == dta & dtau_type == dtt &
                     tau %in% c(0,.3,.6) & Sh %in% Shs],
               formula = Sh + d ~ tau + n,
               value.var = "level")

xdt42 <- dcast(dt2[norm == "Supremum" & S == "I" &
                     n %in% ns & d %in% ds &
                     dtau == dta & dtau_type == dtt &
                     tau %in% c(0,.3,.6) & Sh %in% Shs],
               formula = Sh + d ~ tau + n,
               value.var = "level")


xdt <- rbind(xdt1,xdt12,xdt2,xdt22,xdt3,xdt32,xdt4,xdt42)
xdt
xdt <- cbind(NA,xdt[,2],NA,xdt[,3:5],NA,xdt[,6:8],NA,xdt[,9:11])
print(xtable(xdt,digits=1),include.rownames = F)



xdt <- rbind(xdt1,xdt12,xdt2,xdt22)
xdt
xdt <- cbind(NA,xdt[,2],NA,xdt[,3:5],NA,xdt[,6:8],NA,xdt[,9:11])
print(xtable(xdt,digits=1),include.rownames = F)



xdt <- rbind(xdt3,xdt32,xdt4,xdt42)
xdt
print(xtable(xdt,digits=1),include.rownames = F)
print(xtable(xdt[,-1],digits=1),include.rownames = F)
