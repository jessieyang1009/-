library(jsonlite)
library(quantmod)
library(tidyquant)
library(TTR)
rm(list=ls())
##RSI######
token="346e65f0f6a81622e8d19b02f6acbcc6037ae823"
start="2008-01-01"
end="2020-09-11"
stock=2317
api_url = paste0("http://140.124.93.179:5888/v1/history/stock?token=",token,"&symbol_id=",stock,"&data_column=STK.date,STK.o,STK.h,STK.l,STK.c,STK.v&start_date=",start,"&end_date=",end,"")  
STK = fromJSON(api_url)

STK = as.data.frame(STK)

timevec = strptime( STK[,1], "%Y-%m-%d", tz=Sys.timezone())
timevec = as.POSIXct(timevec)
STK=STK[,2:6]
STK = xts(cbind(as.numeric(STK[,1]),as.numeric(STK[,2])
                ,as.numeric(STK[,3]),as.numeric(STK[,4])
                ,as.numeric(STK[,5])), timevec)
colnames(STK) <- c("Open","High","Low","Close","Volume")
#----------------------------------------------------------#
###### 突破+成交量策略 #####
PL1 = setNames(rep(0,length(Cl(STK))),time(STK))
row9 = Cl(STK) > runMax(lag(Hi(STK)),2) #突破2日高點 就買進
row10 = Vo(STK) > lag(Vo(STK))*2 #成交量大於前一日2倍時
row11 = Cl(STK) < runMin(lag(Lo(STK)),8) #跌破8日低點 就賣出

PZ=long=0
for(m in 9:nrow(STK)){
  if(PZ==0 && row9[m]==1 && row10[m]==1){
    long = as.numeric(Cl(STK)[m]*(1+0.001425))
    PZ=1
  }
  if(PZ!=0 && row11[m]==1){
    PL1[m] = as.numeric(Cl(STK)[m]*(1-(0.001425+0.003))-long)
    PZ=0
  }
}

DD = cumsum(PL1)-cummax(cumsum(PL1))
yRang = range(DD,cumsum(PL1))
plot(DD,type="h",col="darkolivegreen1",ylim=yRang)
par(new=T)
plot(cumsum(PL1),col="cadetblue1",type="h",lwd=2,ylim=yRang)
points(which(DD==0),cummax(cumsum(PL1))[which(DD==0)],pch=4,col="darkcyan")
sum(PL1)
