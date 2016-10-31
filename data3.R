library(httr)

library(car)
library(knitr)
library(dplyr)
library(RCurl)
library(glmnet)


x <- getURL("https://raw.githubusercontent.com/AlanasTrapulionis1994/Econometrics/master/mydatanew2.csv")
fulldata <- read.csv2(text = x, sep = ";")
alldata<-fulldata[,c(2:6)]

alldata1<-na.omit(alldata)
visitors<-as.numeric(alldata1[,"as.numeric.monthly_unique_visitors."])
profit<-as.numeric(alldata1[,"as.numeric.monthly_profit."])
price<-as.numeric(alldata1[,"as.numeric.sell_price."])
age<-trunc(
  as.numeric(
    levels(alldata1[,"as.numeric.age."]))[alldata1[,"as.numeric.age."]]
)
time<-as.Date.factor(alldata1[,"ends_at_vector"])
data1<-data.frame(price,profit,visitors,age, time) 

mod1<-lm(price~profit+visitors+age, data = data1)
outlierTest(mod1)
data_out<-data1[-unique(c(855,1456,1024,135,463,59,418,1316,204,1329,as.numeric(rownames(data1[price>10000,])))),] #visi geri duomenys, bet random

data3 <-(data_out[order(data_out$time , decreasing = TRUE ),])

write.csv(data3, "data3.csv", row.names=FALSE)
