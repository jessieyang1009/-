library(quantmod)
STK= get(getSymbols("HD"))
#chartSeries(STK)
row=Cl(STK)>SMA(Cl(STK),5)
PL=setNames(rep(0,length(Cl(STK))),time(STK))
PZ=long=0
row1=Cl(STK) > runMax(lag(Hi(STK)),5)
row2=Cl(STK) < runMin(lag(Lo(STK)),3)
for (m in 6:nrow(STK)){
  if (row1[m]==1){
    long=long+as.numeric(Cl(STK)[m])
    PZ=PZ+1
  }
  if (PZ>=1 && row2[m]==1){ 
    PL[m]=as.numeric(Cl(STK)[m]*PZ-long)
    PZ=long=0
  }
}
plot(cumsum(PL),type="l",col="red",lwd=2)