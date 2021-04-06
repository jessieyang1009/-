library(jsonlite)
library(quantmod)
library(tidyquant)
token="5bef34390dede3ce112a0edac635521d429c327b"
api_url = paste0("http://140.124.93.179:5888/v1/history/stock?token=",token,"&symbol_id=2382&data_column=STK.date,STK.o,STK.h,STK.l,STK.c,STK.v&start_date=2000-01-01&end_date=2020-12-04")
STK = fromJSON(api_url)
STK = as.data.frame(STK)

timevec = strptime( STK[,1], "%Y-%m-%d", tz=Sys.timezone())
timevec = as.POSIXct(timevec)
STK=STK[,2:6]
STK = xts(cbind(as.numeric(STK[,1]),as.numeric(STK[,2])
                ,as.numeric(STK[,3]),as.numeric(STK[,4])
                ,as.numeric(STK[,5])), timevec)
colnames(STK) <- c("Open","High","Low","Close","Volume")

#long單股買單股賣
par(mfrow=c(3,2))
PL=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
#row1=Cl(STK) > runMax(lag(Hi(STK)),10)
#row2=Cl(STK) < runMin(lag(Lo(STK)),7)
row1=Cl(STK) < runMin(lag(Lo(STK)),30)
row2=Cl(STK) > runMax(lag(Hi(STK)),7)
for (m in 31:nrow(STK)){
  if (PZ==0 && row1[m-1]==0 && row1[m]==1){
    long=as.numeric(Cl(STK)[m])*(1+0.001425)
    PZ=1
  }
  if (PZ==1 && row2[m-1]==0 && row2[m]==1){ 
    PL[m]=as.numeric(Cl(STK)[m]-long)*(1-0.001425-0.003)
    PZ=0
  }
  
}
#plot(cumsum(PL),type="h",col="red",lwd=2)
DD=cumsum(PL)-cummax(cumsum(PL))
yRang=range(DD,cumsum(PL))
plot(DD,type = 'h',col='blue',ylim = yRang)
par(new=T)
plot(cumsum(PL),col='darkgreen',lwd=2,type = 'h',ylim = yRang)
points(which(DD==0),cummax(cumsum(PL))[which(DD==0)],pch=4,col="red")
length(PL[PL>0])/length(PL[PL!=0]) #勝率
mean(PL[PL>0])/abs(mean(PL[PL<0])) #賺賠比
sum(PL[PL>=0])/abs(sum(PL[PL<0])) #獲利因子
####long單股一直買(加碼)，全部賣出
PL=PZ=setNames(rep(0,length(Cl(STK))),time(STK))
long=0
#row1=Cl(STK) > runMax(lag(Hi(STK)),8)
#row2=Cl(STK) < runMin(lag(Lo(STK)),5)
row1=Cl(STK) < runMin(lag(Lo(STK)),30)
row2=Cl(STK) > runMax(lag(Hi(STK)),7)
for (m in 31:nrow(STK)){
  PZ[m]=PZ[m-1]
  if (row1[m]==1){
    long=long+as.numeric(Cl(STK)[m])*(1+0.001425)
    PZ[m]=PZ[m-1]+1
  }
  if (PZ[m-1]!=0 && row2[m]==1){ 
    PL[m]=as.numeric(Cl(STK)[m]*PZ[m]-long)*(1-0.001425-0.003)
    PZ[m]=long=0
  }
  
}
#plot(cumsum(PL1),type="h",col="red",lwd=2)

DD=cumsum(PL)-cummax(cumsum(PL))
yRang=range(DD,cumsum(PL))

plot(DD,type = 'h',col='gold',ylim = yRang)
par(new=T)
plot(cumsum(PL),col='pink',lwd=2,type = 'h',ylim = yRang)
points(which(DD==0),cummax(cumsum(PL))[which(DD==0)],pch=4,col="red")

length(PL[PL>0])/length(PL[PL!=0])#勝率
mean(PL[PL>0])/abs(mean(PL[PL<0]))#賺賠比
sum(PL[PL>=0])/abs(sum(PL[PL<0])) #獲利因子

#View(cbind(as.numeric(Cl(STK)),as.numeric(Lo(STK))
#           ,as.numeric(runMax(lag(Lo(STK)),5)),as.numeric(row1)
#           ,as.numeric(Hi(STK))
#           ,as.numeric(runMax(lag(Hi(STK)),2)),as.numeric(row2)
#           ,PL,PZ))

#short單股
PL1=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
row3=Cl(STK) > runMax(lag(Hi(STK)),5)
row4=Cl(STK) < runMin(lag(Lo(STK)),2)
for (m in 6:nrow(STK)){
  if (PZ==0 && row3[m-1]==0 && row3[m]==1){
    short=as.numeric(Cl(STK)[m])*(1-0.001425-0.003)
    PZ=1
  }
  if (PZ==1 && row4[m-1]==0 && row4[m]==1){ 
    PL1[m]=as.numeric(short-Cl(STK)[m])*(1+0.001425)
    PZ=0
  }
  
}
#plot(cumsum(PL),type="h",col="red",lwd=2)
DD=cumsum(PL1)-cummax(cumsum(PL1))
yRang=range(DD,cumsum(PL1))
plot(DD,type = 'h',col='blue',ylim = yRang)
par(new=T)
plot(cumsum(PL1),col='darkgreen',lwd=2,type = 'h',ylim = yRang)
points(which(DD==0),cummax(cumsum(PL1))[which(DD==0)],pch=4,col="red")
length(PL1[PL1>0])/length(PL1[PL1!=0]) #勝率
mean(PL1[PL1>0])/abs(mean(PL1[PL1<0])) #賺賠比
sum(PL1[PL1>=0])/abs(sum(PL1[PL1<0])) #獲利因子

#View(cbind(as.numeric(Cl(STK)),as.numeric(Lo(STK))
#           ,as.numeric(runMax(lag(Lo(STK)),3)),as.numeric(row4)
#           ,as.numeric(Hi(STK))
#           ,as.numeric(runMax(lag(Hi(STK)),6)),as.numeric(row3)
#           ,PL1))


####short單股加碼，全部賣出
PL1=PZ=setNames(rep(0,length(Cl(STK))),time(STK))
short=0
row3=Cl(STK) > runMax(lag(Hi(STK)),5)
row4=Cl(STK) < runMin(lag(Lo(STK)),2)
for (m in 6:nrow(STK)){
  PZ[m]=PZ[m-1]
  if (row3[m]==1){
    short=short+as.numeric(Cl(STK)[m])*(1-0.001425-0.003)
    PZ[m]=PZ[m-1]+1
  }
  if (PZ[m-1]!=0 && row4[m]==1){ 
    PL1[m]=as.numeric(short-Cl(STK)[m]*PZ[m])*(1+0.001425)
    PZ[m]=short=0
  }
}
#plot(cumsum(PL1),type="h",col="red",lwd=2)

DD=cumsum(PL1)-cummax(cumsum(PL1))
yRang=range(DD,cumsum(PL1))

plot(DD,type = 'h',col='gold',ylim = yRang)
par(new=T)
plot(cumsum(PL1),col='pink',lwd=2,type = 'h',ylim = yRang)
points(which(DD==0),cummax(cumsum(PL1))[which(DD==0)],pch=4,col="red")

length(PL1[PL1>0])/length(PL1[PL1!=0])#勝率
mean(PL1[PL1>0])/abs(mean(PL1[PL1<0]))#賺賠比
sum(PL1[PL1>=0])/abs(sum(PL1[PL1<0])) #獲利因子

#View(cbind(as.numeric(Cl(STK)),as.numeric(Lo(STK))
#           ,as.numeric(runMax(lag(Lo(STK)),3)),as.numeric(row3)
#           ,as.numeric(Hi(STK))
#           ,as.numeric(runMax(lag(Hi(STK)),6)),as.numeric(row4)
#           ,PL1,PZ))

#long&short單股
PL2=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
PZ1=0
#long
row1=Cl(STK) < runMin(lag(Lo(STK)),30)
row2=Cl(STK) > runMax(lag(Hi(STK)),7)
for (m in 31:nrow(STK)){
  if (PZ==0 && row1[m-1]==0 && row1[m]==1){
    long=as.numeric(Cl(STK)[m])*(1+0.001425)
    PZ=1
  }
  if (PZ==1 && row2[m-1]==0 && row2[m]==1){ 
    PL2[m]=as.numeric(Cl(STK)[m]-long)*(1-0.001425-0.003)
    PZ=0
  }
}
#short
row3=Cl(STK) > runMax(lag(Hi(STK)),5)
row4=Cl(STK) < runMin(lag(Lo(STK)),2)
for (m in 6:nrow(STK)){
  if (PZ1==0 && row3[m-1]==0 && row3[m]==1){
    short=as.numeric(Cl(STK)[m])*(1-0.001425-0.003)
    PZ1=1
  }
  if (PZ1==1 && row4[m-1]==0 && row4[m]==1){ 
    PL2[m]=PL2[m]+short-as.numeric(Cl(STK)[m])*(1+0.001425)
    PZ1=0
  }
}

DD=cumsum(PL2)-cummax(cumsum(PL2))
yRang=range(DD,cumsum(PL2))
plot(DD,type = 'h',col='blue',ylim = yRang)
par(new=T)
plot(cumsum(PL2),col='darkgreen',lwd=2,type = 'h',ylim = yRang)
points(which(DD==0),cummax(cumsum(PL2))[which(DD==0)],pch=4,col="red")
length(PL2[PL2>0])/length(PL2[PL2!=0]) #勝率
mean(PL2[PL2>0])/abs(mean(PL2[PL2<0])) #賺賠比
sum(PL2[PL2>=0])/abs(sum(PL2[PL2<0])) #獲利因子

#View(cbind(as.numeric(Cl(STK)),as.numeric(Lo(STK))
#           ,as.numeric(runMax(lag(Lo(STK)),5)),as.numeric(row1)
#           ,as.numeric(Hi(STK))
#           ,as.numeric(runMax(lag(Hi(STK)),2)),as.numeric(row2)
#           ,PL2))


####long&short加碼後全部賣出
PL2=PZ=setNames(rep(0,length(Cl(STK))),time(STK))
long=0
row1=Cl(STK) < runMin(lag(Lo(STK)),30)
row2=Cl(STK) > runMax(lag(Hi(STK)),7)
for (m in 31:nrow(STK)){
  PZ[m]=PZ[m-1]
  if (row1[m]==1){
    long=long+as.numeric(Cl(STK)[m])*(1+0.001425)
    PZ[m]=PZ[m-1]+1
  }
  if (PZ[m-1]!=0 && row2[m]==1){ 
    PL2[m]=as.numeric(Cl(STK)[m]*PZ[m]-long)*(1-0.001425-0.003)
    PZ[m]=long=0
  }
}
short=0
row3=Cl(STK) > runMax(lag(Hi(STK)),5)
row4=Cl(STK) < runMin(lag(Lo(STK)),2)
for (m in 6:nrow(STK)){
  PZ[m]=PZ[m-1]
  if (row3[m]==1){
    short=short+as.numeric(Cl(STK)[m])*(1-0.001425-0.003)
    PZ[m]=PZ[m-1]+1
  }
  if (PZ[m-1]!=0 && row4[m]==1){ 
    PL2[m]=PL2[m]+short-as.numeric(Cl(STK)[m]*PZ[m])*(1+0.001425)
    PZ[m]=short=0
  }
}

DD=cumsum(PL2)-cummax(cumsum(PL2))
yRang=range(DD,cumsum(PL2))

plot(DD,type = 'h',col='gold',ylim = yRang)
par(new=T)
plot(cumsum(PL2),col='pink',lwd=2,type = 'h',ylim = yRang)
points(which(DD==0),cummax(cumsum(PL2))[which(DD==0)],pch=4,col="red")

length(PL2[PL2>0])/length(PL2[PL2!=0])#勝率
mean(PL2[PL2>0])/abs(mean(PL2[PL2<0]))#賺賠比
sum(PL2[PL2>=0])/abs(sum(PL2[PL2<0])) #獲利因子
