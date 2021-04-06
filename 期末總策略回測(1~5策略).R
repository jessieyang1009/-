library(jsonlite)
library(quantmod)
library(tidyquant)
library(TTR)
rm(list=ls())
##RSI######
token="5bef34390dede3ce112a0edac635521d429c327b"
start="2008-01-01"
end="2020-09-11"
stock=3406
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
chartSeries(STK)
addRSI()
rsi_data=na.omit(RSI(Cl(STK)))
#View(rsi_data)

PL1 = setNames(rep(0,length(Cl(STK))),time(STK))
PZ=long=0

row1=rsi_data>50
row2=rsi_data<50

for(m in 15:nrow(STK)){
  if(PZ==0 && row1[m-1]==0 && row1[m]==1){
    long = as.numeric(Cl(STK)[m]*(1+0.001425))
    PZ=1
  }
  if(PZ!=0 && row2[m-1]==0 && row2[m]==1){
    PL1[m] = as.numeric(Cl(STK)[m]*(1-(0.001425+0.003))-long)
    PZ=0
  }
}


#####macd######
#chartSeries(STK)
addMACD()
macd_data=na.omit(MACD(Cl(STK)),percent=F)
# 計算MACD
diff=macd_data$macd
dea=macd_data$signal
macd=-2*(diff-dea)
#View(macd)
#PL1 = setNames(rep(0,length(Cl(STK))),time(STK))
PZ=long=0

row3=macd>0
row4=macd<0
for(m in 34:nrow(STK)){
  if(PZ==0 && row3[m-1]==0 && row3[m]==1){
    long = as.numeric(Cl(STK)[m]*(1+0.001425))
    PZ=1
  }
  if(PZ!=0 && row4[m-1]==0 && row4[m]==1){
    PL1[m] = PL1[m] + as.numeric(Cl(STK)[m]*(1-(0.001425+0.003))-long)
    PZ=0
  }
}
###### 突破+成交量策略 #####

row5 = Cl(STK) > runMax(lag(Hi(STK)),2) #突破2日高點 就買進
row6 = Vo(STK) > lag(Vo(STK))*2 #成交量大於前一日2倍時
row7 = Cl(STK) < runMin(lag(Lo(STK)),8) #跌破8日低點 就賣出

PZ=long=0
for(m in 9:nrow(STK)){
  if(PZ==0 && row5[m]==1 && row6[m]==1){
    long = as.numeric(Cl(STK)[m]*(1+0.001425))
    PZ=1
  }
  if(PZ!=0 && row7[m]==1){
    PL1[m] = PL1[m]+as.numeric(Cl(STK)[m]*(1-(0.001425+0.003))-long)
    PZ=0
  }
}

####bais#######
token="5bef34390dede3ce112a0edac635521d429c327b"
start="2008-01-01"
end="2020-09-11"
stock=2330
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
#PL1 = setNames(rep(0,length(Cl(STK))),time(STK))

row8 = (Cl(STK)-SMA(Cl(STK),10))/SMA(Cl(STK),10) < (-0.045)
row9 = (Cl(STK)-SMA(Cl(STK),10))/SMA(Cl(STK),10) > (-0.045)
row10 = (Cl(STK)-SMA(Cl(STK),10))/SMA(Cl(STK),10) > 0.05
row11 = (Cl(STK)-SMA(Cl(STK),10))/SMA(Cl(STK),10) < 0.05
bias = (Cl(STK)-SMA(Cl(STK),10))/SMA(Cl(STK),10)

#View(sd(Cl(STK)[(m-10):(m-1)]))
#View(mean(sd(Cl(STK))))
#View(sd(Cl(STK)[(m-10):(m-1)]) < mean(sd(Cl(STK))))

PZ=long=0
for(m in 11:nrow(STK)){
  if(PZ==0 && row8[m-1]==1 && row9[m]==1){
      long=as.numeric(Cl(STK)[m]*(1+0.001425))
      PZ=1
    
  }
  if(PZ!=0 && row10[m-1]==1 && row11[m]==1){
    PL1[m]=PL1[m] + as.numeric(Cl(STK)[m]*(1-(0.001425+0.003))-long)
    PZ=0
  }
}


###BBands######
token="5bef34390dede3ce112a0edac635521d429c327b"
start="2008-01-01"
end="2020-09-11"
stock=3045
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

#PL = setNames(rep(0,length(Cl(STK))),time(STK))
row12 = Cl(STK) <= BBands(Cl(STK))[,1]
row13 = Cl(STK) >= BBands(Cl(STK))[,3]

PZ=long=0
for(m in 21:nrow(STK)){ #因為預設20MA
  #布林通道碰到下限 買進
  if(PZ==0 && row12[m-1]==0 && row12[m]==1){
    long=as.numeric(Cl(STK)[m]*(1+0.001425))
    PZ = 1
  }
  
  #碰到10%停損停利 出場
  if((PZ!=0 && as.numeric(Cl(STK)[m]) > long*1.05)){
    #(PZ!=0 && as.numeric(Cl(STK)[m]) < long*0.9)){
    PL1[m]=PL1[m]+as.numeric(Cl(STK)[m]*(1-(0.001425+0.003))-long)
    PZ = 0
    
  }
  #碰到布林通道上限 出場
  if(PZ!=0 && row13[m]==1){
    PL1[m]=PL1[m]+as.numeric(Cl(STK)[m]*(1-(0.001425+0.003))-long)
    PZ = 0
  }
}

DD = cumsum(PL1)-cummax(cumsum(PL1))
yRang = range(DD,cumsum(PL1))
plot(DD,type="h",col="darkolivegreen1",ylim=yRang)
par(new=T)
plot(cumsum(PL1),col="cadetblue1",type="h",lwd=2,ylim=yRang)
points(which(DD==0),cummax(cumsum(PL1))[which(DD==0)],pch=4,col="darkcyan")
sum(PL1)
length(PL1[PL1>0])/length(PL1[PL1!=0])#勝率
mean(PL1[PL1>0])/abs(mean(PL1[PL1<0]))#賺賠比
sum(PL1[PL1>=0])/abs(sum(PL1[PL1<0]))#PF
