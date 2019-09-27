library("rvest")
library("quantmod")
library("data.table")


# Get tickers and metadata ------------------------------------------------

# NASDAQ_tickers <- read.csv("http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download")
# fwrite(NASDAQ_tickers, "application/NASDAQ_tickers.csv")
NASDAQ_tickers <- fread("application/NASDAQ_tickers.csv")

# NASDAQ100_meta <- t(sapply(symbols, function(s){
#   NASDAQ_tickers[which(NASDAQ_tickers[,1] == s),]
# },simplify = FALSE))
# NASDAQ100_meta <- rbindlist(NASDAQ100_meta)
# fwrite(NASDAQ100_meta, "application/NASDAQ100_meta.csv")
NASDAQ100_meta <- fread("application/NASDAQ100_meta.csv")




# Investigate metadata ----------------------------------------------------

str(NASDAQ100_meta)

# sectors
unique(NASDAQ100_meta$Sector)
# tickers by sector
G.sector <- sapply(unique(NASDAQ100_meta$Sector), function(sec){
  NASDAQ100_meta$Symbol[which(NASDAQ100_meta$Sector == sec)]
})
G.sector

# industries
unique(NASDAQ100_meta$industry)
# tickers by industry
G.industry <- sapply(unique(NASDAQ100_meta$industry), function(indus){
  NASDAQ100_meta$Symbol[which(NASDAQ100_meta$industry == indus)]
})
G.industry




# rvest quotes ------------------------------------------------------------

# We rvest the tickers from internet (www.cnbc.com/nasdaq-100) -- might need to be changed if the site is modified
# symbols <- read_html("https://www.cnbc.com/nasdaq-100/") %>%
#   html_nodes(".text a") %>%
#   html_text()


# load historical data with quantmod::getSymbols
# NASDAQ100_quotes <- new.env()
# getSymbols(as.character(NASDAQ100_meta$Symbol), src = 'yahoo', from = '2000-01-01', env = data, auto.assign = T)    
# saveRDS(NASDAQ100_quotes, file = "application/quotes.rds")

NASDAQ100_quotes <- readRDS("application/quotes.rds")
ls(NASDAQ100_quotes)





# Crunch the data as pleased ----------------------------------------------

# find missing dates
# NASDAQ100_close <- rbindlist(lapply(ls(NASDAQ100_quotes), function(tick){
#   dt <- as.data.table(NASDAQ100_quotes[[tick]][,4])
#   names(dt) <- c("date", "quote")
#   dt[, ticker := tick]
# }))
# 
# quotes <- dcast(NASDAQ100_close, date~ticker, value.var = "quote")
# quotes$date
# 
# nas.row <- apply(quotes, 1, function(r) sum(is.na(r)))
# plot(quotes$date, nas.row)
# 
# quotes <- quotes[which(nas.row==0)[1]:nrow(quotes),]
# range(quotes$date)
# 
# fwrite(quotes, "application/quotes_all.csv")

