library(quantmod)
par(mfrow=c(1,2))
STK= get(getSymbols("HD"))
#chartSeries(STK)
row=Cl(STK)>SMA(Cl(STK),5)

#單股買單股賣
PL=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
row1=Cl(STK) > runMax(lag(Hi(STK)),5)
row2=Cl(STK) < runMin(lag(Lo(STK)),3)
for (m in 6:nrow(STK)){
  if (PZ==0 && row1[m-1]==0 && row1[m]==1){#從沒有5日突破變成5日突破
    long=as.numeric(Cl(STK)[m])*(1-0.001425)
    PZ=1
  }
  if (PZ==1 && row2[m-1]==0 && row2[m]==1){ #從沒有三日新低變成三日新低
    PL[m]=as.numeric(Cl(STK)[m]-long)*(1-0.001425-0.003)
    PZ=0
  }
}
plot(cumsum(PL),type="l",col="red",lwd=2)


#單股一直買(加碼)，全部賣出
PL1=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=long=0
row1=Cl(STK) > runMax(lag(Hi(STK)),5)
row2=Cl(STK) < runMin(lag(Lo(STK)),3)
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
plot(cumsum(PL1),type="l",col="red",lwd=2)
