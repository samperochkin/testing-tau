library("data.table")


# turn quotes into weekly returns ----------------------------------------

quotes <- fread("application/quotes_all.csv")

# wednesday is "less missing"... 
table(wday(quotes$date)) 

# in fact there is not much missing
difftime(max(quotes$date),min(quotes$date),units = "weeks")

# so let us pick every Wednesday
quotes <- quotes[wday(date) == 4]
# quotes <- subset(quotes, select = which(NASDAQ100_meta[Symbol %in% colnames(quotes[-1]),Sector]=="Technology"))

# and compute the returns
# returns <- as.matrix(apply(as.matrix(quotes[,-1]), 2, function(v) log(1+diff(v)/v[-length(v)])))
returns <- as.matrix(apply(as.matrix(quotes[,-1]), 2, function(v) log(v[-1]/v[-length(v)])))
rownames(returns) <- as.Date(quotes$date[-1])


saveRDS(returns, "application/returns_mat.rds")


