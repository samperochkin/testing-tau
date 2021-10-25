library(rvest)
psmlr <- read_html("https://www.psmsl.org/data/obtaining/index.php")

stns <- psmlr %>%
  html_node("#stationTable , td, .header") %>%
  html_table()

write_csv(stns,"data\\rlr_monthly\\rvested-stations.csv")

