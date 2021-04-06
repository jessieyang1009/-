library(quantmod)
STK= get(getSymbols("TSLA"))
#chartSeries(STK)
row=Cl(STK)>SMA(Cl(STK),5)  #row是true/false向量

#View(cbind(Cl(STK),SMA(Cl(STK),5),row)) #因為用5ma所以前四天沒有值
PL=setNames(rep(0,length(Cl(STK))),time(STK)) #損益向量（記錄每天賺多少賠多少）

m=4
while (m < nrow(STK)){
  if (Cl(STK[m] > max(Hi(STK)[(m-1):(m-5)]))){ #收盤價突破五日新高買進
    long=as.numeric(Op(STK)[m+1])
    while(Cl(STK[m]>=min(Lo(STK)[(m-1):(m-3)])) 
          && m < nrow(STK)){m=m+1}
    PL[m]=as.numeric(Op(STK)[m+1]-long)
  }
  m=m+1  
}
plot(cumsum(PL),type="l",col="red",lwd=2)

#while改for
PL=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
for (m in 6:nrow(STK)){
  if (PZ==0 && Cl(STK[m] > max(Hi(STK)[(m-1):(m-5)]))){
    #突破五日高點買
    long=as.numeric(Cl(STK)[m])
    PZ=1
  }
  if (PZ==1 && Cl(STK[m])<min(Lo(STK)[(m-1):(m-3)])){
    #跌破三日低點賣
    PL[m]=as.numeric(Cl(STK)[m]-long)
    PZ=0
  }
}
plot(cumsum(PL),type="l",col="red",lwd=2)

############單股買單股賣
PL=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=0
row1=Cl(STK) > runMax(lag(Hi(STK)),3)
row2=Cl(STK) < runMin(lag(Lo(STK)),3)
for (m in 4:nrow(STK)){
  if (PZ==0 && row1[m-1]==0 && row1[m]==1){#從沒有三日突破變成三日突破
    long=as.numeric(Cl(STK)[m])
    PZ=1
  }
if (PZ==1 && row2[m-1]==0 && row2[m]==1){ #從沒有三日新低變成三日新低
  PL[m]=as.numeric(Cl(STK)[m]-long)
  PZ=0
  }
}
plot(cumsum(PL),type="l",col="red",lwd=2)
View(cbind(Hi(STK),lag(Hi(STK)),Cl(STK),row1)) #7/13 GOOG買3.628
View(cbind(Lo(STK),lag(Lo(STK)),Cl(STK),row2)) #7/30 GOOG賣3.988
View(PL) #7/30 賺0.36

##只要三日高點買進，創三日新低全賣
PL=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=long=0
row1=Cl(STK) > runMax(lag(Hi(STK)),3)
row2=Cl(STK) < runMin(lag(Lo(STK)),3)
for (m in 4:nrow(STK)){
  if (row1[m]==1){#從沒有三日突破變成三日突破
    long=long+as.numeric(Cl(STK)[m])
    PZ=PZ+1
  }
  if (PZ>=1 && row2[m]==1){ #從沒有三日新低變成三日新低
    PL[m]=as.numeric(Cl(STK)[m]*PZ-long)
    PZ=long=0
  }
}
plot(cumsum(PL),type="l",col="red",lwd=2)
