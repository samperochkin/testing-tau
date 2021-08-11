con <- file("sim/sim-main/analysisScripts/tables/exch_tables.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

# This will echo all input and not truncate 150+ character lines...
source("sim/sim-main/analysisScripts/tables/main-exch.R", echo=TRUE, max.deparse.length=10000)

# Restore output to console
sink() 
sink(type="message")



con <- file("sim/sim-main/analysisScripts/tables/exch_block_tables.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

# This will echo all input and not truncate 150+ character lines...
source("sim/sim-main/analysisScripts/tables/main-block.R", echo=TRUE, max.deparse.length=10000)

# Restore output to console
sink() 
sink(type="message")
