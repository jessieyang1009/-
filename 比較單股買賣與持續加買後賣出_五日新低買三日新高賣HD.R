library(quantmod)
par(mfrow=c(1,2))
STK= na.omit(get(getSymbols("HD")))
#chartSeries(STK)
row=Cl(STK)>SMA(Cl(STK),5)

#單股買單股賣
PL=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
row1=Cl(STK) < runMax(lag(Lo(STK)),5)
row2=Cl(STK) > runMin(lag(Hi(STK)),3)
for (m in 7:nrow(STK)){
  if (PZ==0 && row1[m-1]==0 && row1[m]==1){
    long=as.numeric(Cl(STK)[m])*(1-0.001425)
    PZ=1
  }
  if (PZ==1 && row2[m-1]==0 && row2[m]==1){ #從沒有三日新高變成三日新高
    PL[m]=as.numeric(Cl(STK)[m]-long)*(1-0.001425-0.003)
    PZ=0
  }
  
}
#plot(cumsum(PL),type="h",col="red",lwd=2)
DD=cumsum(PL)-cummax(cumsum(PL))
yRang=range(DD,cumsum(PL))
plot(DD,type = 'h',col='gold',ylim = yRang)
par(new=T)
plot(cumsum(PL),col='pink',lwd=2,type = 'h',ylim = yRang)
points(which(DD==0),cummax(cumsum(PL))[which(DD==0)],pch=4,col="red")
length(PL[PL>0])/length(PL[PL!=0]) #勝率
mean(PL[PL>0])/abs(mean(PL[PL<0])) #賺賠比
sum(PL[PL>=0])/abs(sum(PL[PL<0])) #獲利因子

#單股一直買(加碼)，全部賣出
PL1=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=long=0
row1=Cl(STK) < runMax(lag(Lo(STK)),5)
row2=Cl(STK) > runMin(lag(Hi(STK)),3)
for (m in 6:nrow(STK)){
  if (row1[m]==1){
    long=long+as.numeric(Cl(STK)[m])*(1-0.001425)
    PZ=PZ+1
  }
  if (PZ>=1 && row2[m]==1){ 
    PL1[m]=as.numeric(Cl(STK)[m]*PZ-long)*(1-0.001425-0.003)
    PZ=long=0
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
