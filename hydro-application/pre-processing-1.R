library(dplyr)
library(tidyr)
library(readr)


# Stations info -----------------------------------------------------------
stns <- read_csv("data/rvested-stations.csv")
stns <- stns %>% subset(stns$Country %in% c("CAN","USA","MEX","CUB","DOM","GRD","SLV",
                                            "GLP","GTM","HTI","HND","JAM","NIC","PAN",
                                            "PRI","KNA","LCA"))
table(stns %>% pull(Country))

write_csv(stns,"data/stns_pp1.csv")
#id <- stns %>% subset(Country == "CAN") %>% select(ID) %>% pull



# Import data -------------------------------------------------------------
data <- as.data.frame(matrix(nrow=0,ncol=5))

for(ii in stns$ID){
  
  fii <- paste0("data/rlr_monthly/data/",ii,".rlrdata")
  if(file.exists(fii)){
    
    temp.data <- read_delim(paste0("data/rlr_monthly/data/",ii,".rlrdata"), 
                            ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE, col_types = cols())
    temp.data$ID <- ii
    data <- rbind(data,temp.data)
  }
}

data <- data %>% select(X1, X2, ID)
names(data) <- c("YEAR", "MEAN_WL", "ID")

data <- data %>% subset(data$YEAR - floor(data$YEAR) == .125) # keep February only (2nd month)
data$YEAR <- floor(data$YEAR)
data$MEAN_WL[data$MEAN_WL == -99999] <- NA


# Select stations ---------------------------------------------------------

y0 <- 2018 - 69 # consider the last 70 years (from 2018, to include 3-rivers)
y1 <- 2018

data2 <- data %>% subset(between(data$YEAR,y0,y1)) # keep only relevant years
ids <- data2 %>% group_by(ID) %>% summarise(num = sum(is.na(MEAN_WL))) %>% subset(num <= 20) %>% pull(ID)
data2 <- data2 %>% subset(ID %in% ids) # keep only stations with at most 10 missing observations

# construct data frame
data2 <- as.data.frame(data2 %>% spread(key=ID, value=MEAN_WL, fill=NA))

# find good balance between number of variables and number of observations
last.na <- apply(data2[,-1], 2, function(x){
  la <- rev(which(is.na(x)))[1]
  if(is.na(la)) return(0)
  la
})

par(mar=c(4,4,1,1))
plot(0:20,sapply(1:21, function(k){
  sum(last.na < k)
}),
xlab="number of observations removed",
ylab="number of stations with complete data")
abline(v=1, lty=2)
abline(v=5, lty=2)
abline(v=7, lty=2)
abline(v=10, lty=2)


data2 <- data2[-(1:5),] # remove 5 first years
data2 <- data2 %>% select(which(apply(!is.na(data2),2,all))) # keep only full columns
plot(data2$YEAR) # yeah, all years are consecutive

write_csv(data2,"data/data_pp1.csv")
