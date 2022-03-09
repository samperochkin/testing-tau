

# Packages ----------------------------------------------------------------

library(data.table)
library(ggplot2)



# Load results ------------------------------------------------------------

filenames <- list.files("sim/sim-main/powerCurves/results", recursive = T, full.names = T)
filenames <- filenames[grepl("5-3", filenames)]
full.grid <- rbindlist(lapply(filenames, fread))
# fwrite(full.grid, "sim/sim-main/powerCurves/results-3.csv")
fwrite(full.grid, "powerCurves/results-3.csv")
