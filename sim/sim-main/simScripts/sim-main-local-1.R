
# packages ----------------------------------------------------------------
library(data.table)

# functions ---------------------------------------------------------------
source("simFuns/simFunLocal.R")

# procedure ---------------------------------------------------------------

tiime <- Sys.time()
simFunLocal(n = c(50,250,450),
          d = c(3),
          tau = c(.2,.4,.6),
          epsilon = c(.1,.3,.5),
          distribution = c("normal"),
          num_sim = 1000,
          filename = "dt_main_local_2",
          cores = 8
)
difftime(Sys.time(),tiime)
