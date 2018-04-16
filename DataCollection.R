library(RCurl)
library(rjson)
library(anytime)
library(tidyverse)
library(lubridate)
library(fPortfolio)

BTC <- fromJSON(readLines('Price.txt'))
BTC <- do.call(rbind.data.frame, BTC)
BTC$date<-anytime(BTC$date)
BTC$day<-as.Date(BTC$date,format='%m/%d/%Y')
BTC.price <-BTC %>% group_by(day) %>%  summarise(close=mean(close),
                                                volume=mean(volume))
#Read Gini by Date
gini<-read.csv("dates_giniz.txt")
#Convert to date
gini$day<- as.Date(as.character(gini$date), "%Y-%m-%d")

df<- BTC.price %>% 
     full_join(gini,by="day")

write.csv(gini,"gini2.csv")
write.csv(BTC.price,"BTC.price2.csv")
