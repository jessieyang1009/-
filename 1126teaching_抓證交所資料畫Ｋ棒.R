library(quantmod)
rm(list = ls())
setwd("/Users/yangyichen/Desktop/data")
STK=read.csv("STOCK_DAY_2395_202006.csv",skip=1 , 
             header = TRUE,fileEncoding = "big5")

STK=STK[(-nrow(STK)):(-nrow(STK)+3),] #delete 後三行
STK[,2]=as.numeric(gsub(",","",STK[,2]))#將逗點移除換成空值
STK[,3]=as.numeric(gsub(",","",STK[,3]))
STK[,9]=as.numeric(gsub(",","",STK[,9]))

#將民國改成西元
timeCharVactor=paste(STK[,1])
timeCharVactor=gsub("/","-",timeCharVactor)
TimeY=substr(timeCharVactor,1,3)
TimeY=as.numeric(TimeY)+1911
timeCharVactor=paste0(TimeY,substr(timeCharVactor,4,9))
#改成Ｒ可使用的
timeVector=strptime(timeCharVactor,"%Y-%m-%d",tz=Sys.timezone())
timeVector=as.POSIXct(timeVector)

STK=as.matrix(STK[,c(4,5,6,7,9)])
colnames(STK)=c("Open","High","Low","Close","Volume")
STK=xts(STK,timeVector)
chartSeries(STK)

View(STK)
timeCharVactor
timeVector